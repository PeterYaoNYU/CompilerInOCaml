#include "copying_gc.h"
#include "log.h"

#include <errno.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
//#include "primes.h"

/*
 * Set log level for this compilation unit. If set to LOGLEVEL_DEBUG,
 * the garbage collector will be very chatty.
 */
#undef LOGLEVEL
// #define LOGLEVEL LOGLEVEL_INFO
#define LOGLEVEL LOGLEVEL_DEBUG

/*
 * The size of a pointer.
 */
#define PTRSIZE sizeof(char*)

/*
 * Allocations can temporarily be tagged as "marked" an part of the
 * mark-and-sweep implementation or can be tagged as "roots" which are
 * not automatically garbage collected. The latter allows the implementation
 * of global variables.
 */
#define GC_TAG_NONE 0x0
#define GC_TAG_ROOT 0x1
#define GC_TAG_MARK 0x2

/*
 * Define a globally available GC object; this allows all code that
 * includes the gc.h header to access a global static garbage collector.
 * Convenient for single-threaded code, insufficient for multi-threaded
 * use cases. Use the GC_NO_GLOBAL_GC flag to toggle.
 */
#ifndef GC_NO_GLOBAL_GC
GarbageCollector gc; // global GC object
#endif


static bool is_prime(size_t n)
{
    if (n <= 3)
        return n > 1;     // as 2 and 3 are prime
    else if (n % 2==0 || n % 3==0)
        return false;     // check if n is divisible by 2 or 3
    else {
        for (size_t i=5; i*i<=n; i+=6) {
            if (n % i == 0 || n%(i + 2) == 0)
                return false;
        }
        return true;
    }
}

static size_t next_prime(size_t n)
{
    while (!is_prime(n)) ++n;
    return n;
}

/**
 * The allocation object.
 *
 * The allocation object holds all metadata for a memory location
 * in one place.
 */
typedef struct Allocation {
    void* ptr;                // mem pointer
    size_t size;              // allocated size in bytes
    char tag;                 // the tag for mark-and-sweep
    void (*dtor)(void*);      // destructor
    struct Allocation* next;  // separate chaining
} Allocation;

/**
 * Create a new allocation object.
 *
 * Creates a new allocation object using the system `malloc`.
 *
 * @param[in] ptr The pointer to the memory to manage.
 * @param[in] size The size of the memory range pointed to by `ptr`.
 * @param[in] dtor A pointer to a destructor function that should be called
 *                 before freeing the memory pointed to by `ptr`.
 * @returns Pointer to the new allocation instance.
 */
static Allocation* gc_allocation_new(void* ptr, size_t size, void (*dtor)(void*))
{
    Allocation* a = (Allocation*) malloc(sizeof(Allocation));
    a->ptr = ptr;
    a->size = size;
    a->tag = GC_TAG_NONE;
    a->dtor = dtor;
    a->next = NULL;
    return a;
}

/**
 * Delete an allocation object.
 *
 * Deletes the allocation object pointed to by `a`, but does *not*
 * free the memory pointed to by `a->ptr`.
 *
 * @param a The allocation object to delete.
 */
static void gc_allocation_delete(Allocation* a)
{
    free(a);
}

/**
 * The allocation hash map.
 *
 * The core data structure is a hash map that holds the allocation
 * objects and allows O(1) retrieval given the memory location. Collision
 * resolution is implemented using separate chaining.
 */
typedef struct AllocationMap {
    size_t capacity;
    size_t min_capacity;
    double downsize_factor;
    double upsize_factor;
    double sweep_factor;
    size_t sweep_limit;
    size_t size;
    Allocation** allocs;
} AllocationMap;


void gc_init_heap(GarbageCollector *gc, size_t total_size) {
    void * heap_space = malloc(total_size);
    gc->heap.from_space = heap_space;
    gc->heap.to_space = (void *) ((char *) heap_space + total_size / 2);
    gc->heap.from_space_end = gc->heap.to_space;
    gc->heap.to_space_end = (void *) ((char *) heap_space + total_size);
    gc->heap.allocation_pointer = gc->heap.from_space;
    gc->heap.half_size = total_size / 2;
}

/**
 * Determine the current load factor of an `AllocationMap`.
 *
 * Calculates the load factor of the hash map as the quotient of the size and
 * the capacity of the hash map.
 *
 * @param am The allocationo map to calculate the load factor for.
 * @returns The load factor of the allocation map `am`.
 */
static double gc_allocation_map_load_factor(AllocationMap* am)
{
    return (double) am->size / (double) am->capacity;
}

static AllocationMap* gc_allocation_map_new(size_t min_capacity,
        size_t capacity,
        double sweep_factor,
        double downsize_factor,
        double upsize_factor)
{
    AllocationMap* am = (AllocationMap*) malloc(sizeof(AllocationMap));
    am->min_capacity = next_prime(min_capacity);
    am->capacity = next_prime(capacity);
    if (am->capacity < am->min_capacity) am->capacity = am->min_capacity;
    am->sweep_factor = sweep_factor;
    am->sweep_limit = (int) (sweep_factor * am->capacity);
    am->downsize_factor = downsize_factor;
    am->upsize_factor = upsize_factor;
    am->allocs = (Allocation**) calloc(am->capacity, sizeof(Allocation*));
    am->size = 0;
    LOG_DEBUG("Created allocation map (cap=%ld, siz=%ld)", am->capacity, am->size);
    return am;
}

static void gc_allocation_map_delete(AllocationMap* am)
{
    // Iterate over the map
    LOG_DEBUG("Deleting allocation map (cap=%ld, siz=%ld)",
              am->capacity, am->size);
    Allocation *alloc, *tmp;
    for (size_t i = 0; i < am->capacity; ++i) {
        if ((alloc = am->allocs[i])) {
            // Make sure to follow the chain inside a bucket
            while (alloc) {
                tmp = alloc;
                alloc = alloc->next;
                // free the management structure
                gc_allocation_delete(tmp);
            }
        }
    }
    free(am->allocs);
    free(am);
}

static size_t gc_hash(void *ptr)
{
    return ((uintptr_t)ptr) >> 3;
}

static void gc_allocation_map_resize(AllocationMap* am, size_t new_capacity)
{
    if (new_capacity <= am->min_capacity) {
        LOG_DEBUG("Ignoring resize request to %ld (min=%ld)", new_capacity, am->min_capacity);
        return;
    }
    // Replaces the existing items array in the hash table
    // with a resized one and pushes items into the new, correct buckets
    LOG_DEBUG("Resizing allocation map (cap=%ld, siz=%ld) -> (cap=%ld)",
              am->capacity, am->size, new_capacity);
    Allocation** resized_allocs = calloc(new_capacity, sizeof(Allocation*));

    for (size_t i = 0; i < am->capacity; ++i) {
        Allocation* alloc = am->allocs[i];
        while (alloc) {
            Allocation* next_alloc = alloc->next;
            size_t new_index = gc_hash(alloc->ptr) % new_capacity;
            printf("test hash: %ld\n", new_index);
            alloc->next = resized_allocs[new_index];
            resized_allocs[new_index] = alloc;
            alloc = next_alloc;
        }
    }
    free(am->allocs);
    am->capacity = new_capacity;
    am->allocs = resized_allocs;
    LOG_DEBUG("Resized allocation map, updating sweep limit (cap=%ld, siz=%ld)", am->capacity, am->size);
    am->sweep_limit = am->size + am->sweep_factor * (am->capacity - am->size);
    LOG_DEBUG("Resized allocation map complete (cap=%ld, siz=%ld)", am->capacity, am->size);
}

static bool gc_allocation_map_resize_to_fit(AllocationMap* am)
{
    double load_factor = gc_allocation_map_load_factor(am);
    if (load_factor > am->upsize_factor) {
        LOG_DEBUG("Load factor %0.3g > %0.3g. Triggering upsize.",
                  load_factor, am->upsize_factor);
        gc_allocation_map_resize(am, next_prime(am->capacity * 2));
        return true;
    }
    if (load_factor < am->downsize_factor) {
        LOG_DEBUG("Load factor %0.3g < %0.3g. Triggering downsize.",
                  load_factor, am->downsize_factor);
        gc_allocation_map_resize(am, next_prime(am->capacity / 2));
        return true;
    }
    return false;
}

static Allocation* gc_allocation_map_get(AllocationMap* am, void* ptr)
{
    size_t index = gc_hash(ptr) % am->capacity;
    // printf("test hash: %ld\n", index);
    Allocation* cur = am->allocs[index];
    while(cur) {
        if (cur->ptr == ptr) {
            return cur;
        }
        cur = cur->next;
    }
    return NULL;
}

static Allocation* gc_allocation_map_put(AllocationMap* am,
        void* ptr,
        size_t size,
        void (*dtor)(void*))
{
    size_t index = gc_hash(ptr) % am->capacity;
    LOG_DEBUG("PUT request for allocation ix=%ld, size=%ld", index, size);
    Allocation* alloc = gc_allocation_new(ptr, size, dtor);
    printf("alloc size: %ld\n", alloc->size);
    Allocation* cur = am->allocs[index];
    Allocation* prev = NULL;
    /* Upsert if ptr is already known (e.g. dtor update). */
    while(cur != NULL) {
        if (cur->ptr == ptr) {
            // found it
            alloc->next = cur->next;
            if (!prev) {
                // position 0
                am->allocs[index] = alloc;
            } else {
                // in the list
                prev->next = alloc;
            }
            gc_allocation_delete(cur);
            LOG_DEBUG("AllocationMap Upsert at ix=%ld", index);
            return alloc;

        }
        prev = cur;
        cur = cur->next;
    }
    /* Insert at the front of the separate chaining list */
    cur = am->allocs[index];
    alloc->next = cur;
    am->allocs[index] = alloc;
    am->size++;
    LOG_DEBUG("AllocationMap insert at ix=%ld", index);
    void* p = alloc->ptr;
    if (gc_allocation_map_resize_to_fit(am)) {
        LOG_DEBUG("Resized allocation map to fit", "");
        alloc = gc_allocation_map_get(am, p);
        LOG_DEBUG("Get the new alloc", "");
    }
    return alloc;
}


static void gc_allocation_map_remove(AllocationMap* am,
                                     void* ptr,
                                     bool allow_resize)
{
    // ignores unknown keys
    size_t index = gc_hash(ptr) % am->capacity;
    Allocation* cur = am->allocs[index];
    Allocation* prev = NULL;
    Allocation* next;
    while(cur != NULL) {
        next = cur->next;
        if (cur->ptr == ptr) {
            // found it
            if (!prev) {
                // first item in list
                am->allocs[index] = cur->next;
            } else {
                // not the first item in the list
                prev->next = cur->next;
            }
            gc_allocation_delete(cur);
            am->size--;
        } else {
            // move on
            prev = cur;
        }
        cur = next;
    }
    if (allow_resize) {
        gc_allocation_map_resize_to_fit(am);
    }
}


static void* gc_mcalloc(size_t count, size_t size)
{
    if (!count) return malloc(size);
    return calloc(count, size);
}

static bool gc_needs_sweep(GarbageCollector* gc)
{
    return gc->allocs->size > gc->heap.half_size;
}

static void* gc_allocate(GarbageCollector* gc, size_t count, size_t size, void(*dtor)(void*))
{
    size_t alloc_size = count ? count * size : size;

    /* Allocation logic that generalizes over malloc/calloc. */
    void * alloc_ptr = gc->heap.allocation_pointer;
    void * next_alloc_ptr = (void *) ((char *) alloc_ptr + alloc_size);

    if ( next_alloc_ptr > gc->heap.from_space_end) {
        LOG_DEBUG("!!!Out of memory, initiating GC run%s", "");
        gc_run(gc);
        alloc_ptr = gc->heap.allocation_pointer;
        next_alloc_ptr = (char *) alloc_ptr + size;
        if (next_alloc_ptr > gc->heap.to_space_end) {
            LOG_CRITICAL("Out of memory%s", "");
            // potentially, expand the semheap here. For now, just return NULL.
            return NULL;
        }
    }

    gc->heap.allocation_pointer = next_alloc_ptr;
    gc_allocation_map_put(gc->allocs, alloc_ptr, alloc_size, NULL);
    LOG_DEBUG("Allocated %ld bytes @ %p", alloc_size, alloc_ptr);
    return alloc_ptr;
}

static void gc_make_root(GarbageCollector* gc, void* ptr)
{
    Allocation* alloc = gc_allocation_map_get(gc->allocs, ptr);
    if (alloc) {
        alloc->tag |= GC_TAG_ROOT;
    }
}

void* gc_malloc(GarbageCollector* gc, size_t size)
{
    return gc_malloc_ext(gc, size, NULL);
}

void* gc_malloc_static(GarbageCollector* gc, size_t size, void(*dtor)(void*))
{
    void* ptr = gc_malloc_ext(gc, size, dtor);
    gc_make_root(gc, ptr);
    return ptr;
}

void* gc_make_static(GarbageCollector* gc, void* ptr)
{
    gc_make_root(gc, ptr);
    return ptr;
}

void* gc_malloc_ext(GarbageCollector* gc, size_t size, void(*dtor)(void*))
{
    return gc_allocate(gc, 0, size, dtor);
}


void* gc_calloc(GarbageCollector* gc, size_t count, size_t size)
{
    return gc_calloc_ext(gc, count, size, NULL);
}


void* gc_calloc_ext(GarbageCollector* gc, size_t count, size_t size,
                    void(*dtor)(void*))
{
    return gc_allocate(gc, count, size, dtor);
}


void* gc_realloc(GarbageCollector* gc, void* p, size_t size)
{
    Allocation* alloc = gc_allocation_map_get(gc->allocs, p);
    if (p && !alloc) {
        // the user passed an unknown pointer
        errno = EINVAL;
        return NULL;
    }
    void* q = realloc(p, size);
    if (!q) {
        // realloc failed but p is still valid
        return NULL;
    }
    if (!p) {
        // allocation, not reallocation
        Allocation* alloc = gc_allocation_map_put(gc->allocs, q, size, NULL);
        return alloc->ptr;
    }
    if (p == q) {
        // successful reallocation w/o copy
        alloc->size = size;
    } else {
        // successful reallocation w/ copy
        void (*dtor)(void*) = alloc->dtor;
        gc_allocation_map_remove(gc->allocs, p, true);
        gc_allocation_map_put(gc->allocs, q, size, dtor);
    }
    return q;
}

void gc_free(GarbageCollector* gc, void* ptr)
{
    Allocation* alloc = gc_allocation_map_get(gc->allocs, ptr);
    if (alloc) {
        if (alloc->dtor) {
            alloc->dtor(ptr);
        }
        free(ptr);
        gc_allocation_map_remove(gc->allocs, ptr, true);
    } else {
        LOG_WARNING("Ignoring request to free unknown pointer %p", (void*) ptr);
    }
}

void gc_start(GarbageCollector* gc, void* bos)
{
    gc_start_ext(gc, bos, 1024, 1024, 0.2, 0.8, 0.5);
}

void gc_start_ext(GarbageCollector* gc,
                  void* bos,
                  size_t initial_capacity,
                  size_t min_capacity,
                  double downsize_load_factor,
                  double upsize_load_factor,
                  double sweep_factor)
{
    double downsize_limit = downsize_load_factor > 0.0 ? downsize_load_factor : 0.2;
    double upsize_limit = upsize_load_factor > 0.0 ? upsize_load_factor : 0.8;
    sweep_factor = sweep_factor > 0.0 ? sweep_factor : 0.5;
    gc->paused = false;
    gc->bos = bos;
    initial_capacity = initial_capacity < min_capacity ? min_capacity : initial_capacity;
    gc->allocs = gc_allocation_map_new(min_capacity, initial_capacity,
                                       sweep_factor, downsize_limit, upsize_limit);
    gc_init_heap(gc, initial_capacity);
    LOG_DEBUG("Created new garbage collector (cap=%ld, siz=%ld).", gc->allocs->capacity,
              gc->allocs->size);
}

void gc_pause(GarbageCollector* gc)
{
    gc->paused = true;
}

void gc_resume(GarbageCollector* gc)
{
    gc->paused = false;
}

void gc_mark_alloc(GarbageCollector* gc, void* ptr)
{
    Allocation* alloc = gc_allocation_map_get(gc->allocs, ptr);
    /* Mark if alloc exists and is not tagged already, otherwise skip */
    if (alloc && !(alloc->tag & GC_TAG_MARK)) {
        LOG_DEBUG("Marking allocation (ptr=%p)", ptr);
        alloc->tag |= GC_TAG_MARK;
        /* Iterate over allocation contents and mark them as well */
        LOG_DEBUG("Checking allocation (ptr=%p, size=%lu) contents", ptr, alloc->size);
        for (char* p = (char*) alloc->ptr;
                p <= (char*) alloc->ptr + alloc->size - PTRSIZE;
                ++p) {
            LOG_DEBUG("Checking allocation (ptr=%p) @%lu with value %p",
                      ptr, p-((char*) alloc->ptr), *(void**)p);
            gc_mark_alloc(gc, *(void**)p);
        }
    }
}

void gc_mark_stack(GarbageCollector* gc)
{
    LOG_DEBUG("Marking the stack (gc@%p) in increments of %ld", (void*) gc, sizeof(char));
    void *tos = __builtin_frame_address(0);
    void *bos = gc->bos;
    /* The stack grows towards smaller memory addresses, hence we scan tos->bos.
     * Stop scanning once the distance between tos & bos is too small to hold a valid pointer */
    for (char* p = (char*) tos; p <= (char*) bos - PTRSIZE; ++p) {
        gc_mark_alloc(gc, *(void**)p);
    }
}

void gc_mark_roots(GarbageCollector* gc)
{
    LOG_DEBUG("Marking roots%s", "");
    for (size_t i = 0; i < gc->allocs->capacity; ++i) {
        Allocation* chunk = gc->allocs->allocs[i];
        while (chunk) {
            if (chunk->tag & GC_TAG_ROOT) {
                LOG_DEBUG("Marking root @ %p", chunk->ptr);
                gc_mark_alloc(gc, chunk->ptr);
            }
            chunk = chunk->next;
        }
    }
}

void gc_mark(GarbageCollector* gc)
{
    /* Note: We only look at the stack and the heap, and ignore BSS. */
    LOG_DEBUG("Initiating GC mark (gc@%p)", (void*) gc);
    /* Scan the heap for roots */
    gc_mark_roots(gc);
    /* Dump registers onto stack and scan the stack */
    void (*volatile _mark_stack)(GarbageCollector*) = gc_mark_stack;
    jmp_buf ctx;
    memset(&ctx, 0, sizeof(jmp_buf));
    // the setjmp function effectively dumps the registers onto the stack, by saving them into a variable called 
    // ctx, and then returns 0.
    setjmp(ctx);
    _mark_stack(gc);
}

size_t gc_sweep(GarbageCollector* gc)
{
    LOG_DEBUG("Initiating GC sweep (gc@%p)", (void*) gc);
    size_t total = 0;
    for (size_t i = 0; i < gc->allocs->capacity; ++i) {
        Allocation* chunk = gc->allocs->allocs[i];
        Allocation* next = NULL;
        /* Iterate over separate chaining */
        while (chunk) {
            if (chunk->tag & GC_TAG_MARK) {
                LOG_DEBUG("Found used allocation %p (ptr=%p)", (void*) chunk, (void*) chunk->ptr);
                /* unmark */
                chunk->tag &= ~GC_TAG_MARK;
                chunk = chunk->next;
            } else {
                LOG_DEBUG("Found unused allocation %p (%lu bytes @ ptr=%p)", (void*) chunk, chunk->size, (void*) chunk->ptr);
                /* no reference to this chunk, hence delete it */
                total += chunk->size;
                if (chunk->dtor) {
                    chunk->dtor(chunk->ptr);
                }
                free(chunk->ptr);
                /* and remove it from the bookkeeping */
                next = chunk->next;
                gc_allocation_map_remove(gc->allocs, chunk->ptr, false);
                chunk = next;
            }
        }
    }
    gc_allocation_map_resize_to_fit(gc->allocs);
    return total;
}

/**
 * Unset the ROOT tag on all roots on the heap.
 *
 * @param gc A pointer to a garbage collector instance.
 */
void gc_unroot_roots(GarbageCollector* gc)
{
    LOG_DEBUG("Unmarking roots%s", "");
    for (size_t i = 0; i < gc->allocs->capacity; ++i) {
        Allocation* chunk = gc->allocs->allocs[i];
        while (chunk) {
            if (chunk->tag & GC_TAG_ROOT) {
                chunk->tag &= ~GC_TAG_ROOT;
            }
            chunk = chunk->next;
        }
    }
}

size_t gc_stop(GarbageCollector* gc)
{
    gc_unroot_roots(gc);
    size_t collected = gc_sweep(gc);
    gc_allocation_map_delete(gc->allocs);
    return collected;
}

// helper functions for stop and copy
bool isPointerToFromSpace(GarbageCollector * gc, void * ptr) {
    return ptr >= gc->heap.from_space && ptr < gc->heap.from_space_end;
}

bool isPointerToToSpace(GarbageCollector * gc, void * ptr) {
    return ptr >= gc->heap.to_space && ptr < gc->heap.to_space_end;
}

size_t getObjectSize(GarbageCollector * gc, void * ptr) {
    Allocation * alloc = gc_allocation_map_get(gc->allocs, ptr);
    if (!alloc) {
        return 0;
    }
    return alloc->size;
}




/**
 * Forward a pointer to the new location in the to space.
 *
 * @param gc The garbage collector
 * @param ptr The pointer to the memory to manage.
 */
void * forward(GarbageCollector * gc, void * ptr) {
    // Check if the pointer is in the from space
    // if not, it has already been copied, or is not a pointer at all
    if (!isPointerToFromSpace(gc, ptr)) {
        return ptr;
    }

    // if the object has already been copied, return the new pointer
    // its first address should be a forwarding address
    void ** f1 = (void **) ptr;
    if (isPointerToToSpace(gc, *f1)) {
        LOG_DEBUG("Object has already been copied, returning forwarding address %p", *f1);
        return *f1;
    } // object has already been copied, return the forwarding address
    else 
    {
        size_t size = getObjectSize (gc, ptr);
        void * new_location = gc->heap.allocation_pointer;
        memcpy(new_location, ptr, size);
        gc->heap.allocation_pointer = (void *) ((char *) gc->heap.allocation_pointer + size);
        LOG_DEBUG("*Original Location + 0: %p", (*(void **)ptr));
        LOG_DEBUG("*Original Location + 1: %p", (*( (void **)ptr + 1)) );
        LOG_DEBUG("*Original Location + 2: %p", (*((void **)ptr + 2)) );
        LOG_DEBUG("*Original Location + 3: %p", (*((void **)ptr + 3)) );
        LOG_DEBUG("*new location + 0: %p", (*(void **)new_location));
        LOG_DEBUG("*new location + 1: %p", (*( (void **)new_location + 1)) );
        LOG_DEBUG("*new location + 2: %p", (*( (void **)new_location + 2)) );
        LOG_DEBUG("*new location + 3: %p", (*( (void **)new_location + 3)) );

        // install the forwarding address
        *f1 = new_location;
        return new_location;
    }
}

void copyObject(GarbageCollector *gc, void ***scan) {
    void ** object = *scan;
    LOG_DEBUG("Scan starting at %p", *object);

    // for testing purpose only
    Allocation * alloc = gc_allocation_map_get(gc->allocs, *object);
    if (!alloc) {
        return;
    }
    alloc->tag &= GC_TAG_MARK;
    size_t object_size = alloc->size;
    LOG_DEBUG("Copying object at %p with size %zu", *object, object_size);

    // size_t object_size = getObjectSize(gc, object);

    for (size_t i = 0; i < object_size; i += sizeof(void *)) {
        void * field = *(void **)object + i;
        void * new_field = forward(gc, field);
        memcpy(field, &new_field, sizeof(void *));
    }

    LOG_DEBUG("Scan is now at %p", *scan);
    *scan = (void **)((char *)(*scan) + PTRSIZE);
    LOG_DEBUG("after incr Scan is now at %p", *scan);
}

void garbageCollect(GarbageCollector *gc) {
    void ** scan = gc->heap.to_space;
    gc->heap.allocation_pointer = gc->heap.to_space;

    // forward all the roots
    LOG_DEBUG("Forwarding roots%s", "");
    for (size_t i = 0; i < gc->allocs->capacity; ++i) {
        Allocation* chunk = gc->allocs->allocs[i];
        while (chunk) {
            if (chunk->tag & GC_TAG_ROOT) {
                // this marking is for testing purpose only 
                chunk->tag &= GC_TAG_MARK;
                void * new_location = forward(gc, chunk->ptr);
                chunk->ptr = new_location;
            }
            chunk = chunk->next;
        }
    }

    // What is missing: copying the pointers in the stack and the registers
    // with our current implementation, we don't make them roots
    LOG_DEBUG("Forwarding stack%s", "");
    void * tos = __builtin_frame_address(0);
    void * bos = gc->bos;
    for (char* p = (char*) tos; p <= (char*) bos - PTRSIZE; ++p) {
        Allocation* alloc = gc_allocation_map_get(gc->allocs, *(void**)p);
        if (alloc) {
            // this marking is for testing purpose only
            alloc->tag &= GC_TAG_MARK;

            LOG_DEBUG("Forwarding stack pointer %p", *(void**)p);
            void * new_location = forward(gc, alloc->ptr);
            *(void**)p = new_location;
            LOG_DEBUG("Forwarded stack pointer's new location %p", *(void**)p);
        }
    }
    LOG_DEBUG("Forwarding stack complete%s", "");

    void *** scan_ptr = &scan;
    // forward all the objects in the to space
    while (*scan_ptr < gc->heap.allocation_pointer) {
        LOG_DEBUG("Copying object at %p, and the scan ptr is at %p", **scan_ptr ,*scan_ptr);
        LOG_DEBUG("*scan + 1: %p, scan + 1: %p", *(*scan_ptr + 1), *scan_ptr + 1);
        LOG_DEBUG("*Scan + 2: %p, scan + 1: %p", *(*scan_ptr + 2), *scan_ptr + 2);
        copyObject(gc, scan_ptr);  
    }

    LOG_DEBUG("Copying objects complete%s", "");

    // swap the semi-spaces
    void* temp = gc->heap.from_space;
    gc->heap.from_space = gc->heap.to_space;
    gc->heap.to_space = temp;
    // gc->heap.allocation_pointer = gc->heap.to_space;
}

size_t gc_run(GarbageCollector* gc)
{
    LOG_DEBUG("Initiating GC run (gc@%p)", (void*) gc);
    memset(gc->heap.to_space, 0, (size_t)(gc->heap.to_space_end - gc->heap.to_space));

    garbageCollect(gc);
}

char* gc_strdup (GarbageCollector* gc, const char* s)
{
    size_t len = strlen(s) + 1;
    void *new = gc_malloc(gc, len);

    if (new == NULL) {
        return NULL;
    }
    return (char*) memcpy(new, s, len);
}