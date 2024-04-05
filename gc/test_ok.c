#include <stdlib.h> 
#include <stdio.h>
#include "./src/gc.c"
typedef void* (*FunctionPointer)(void *, GarbageCollector);
void* t8(void* dyenv, GarbageCollector gc_) {
   void* result = 0; {
     void* t9 = 0; {
       result = (void*) ((void **) *(void **) *((void**)dyenv+1));
       t9 = (int) ((int)result);
       result = (void*) ((void **) *(void**)dyenv);
       result = (int) ((int)t9+(int)result);
     }
     return (void*)result;
   }
}
void* t7(void* dyenv, GarbageCollector gc_) {
   void* result = 0; {
     result = (void*) (gc_malloc(&gc_,16));
     *(FunctionPointer*)result = ((FunctionPointer)t8);
     *((void**)result+1) = (void*)dyenv;
     return (void*)result;
   }
}
void* main() {
   void* dyenv = 0; {
     void* result = 0; {
      GarbageCollector gc_;
      void *bos = __builtin_frame_address(0);
      gc_start(&gc_, bos);
       void* t1 = 0; {
         void* t2 = 0; {
           void* t3 = 0; {
             void* t4 = 0; {
               void* t5 = 0; {
                 void* t6 = 0; {
                   result = (void*) (gc_malloc(&gc_,16));
                   *(FunctionPointer*)result = ((FunctionPointer)t7);
                   *((void**)result+1) = (void*)dyenv;
                   t4 = (FunctionPointer) ((void **) *(void**)result);
                   t5 = (void*) ((void **) *((void**)result+1));
                   result = (int) (42);
                   t6 = (void*) (gc_malloc(&gc_,16));
                   *(void**)t6 = (void*)result;
                   *((void**)t6+1) = (void*)t5;
                   result = (void*) (((FunctionPointer)t4)((void*)t6,gc_));
                 }
               }
             }
             t1 = (FunctionPointer) ((void **) *(void**)result);
             t2 = (void*) ((void **) *((void**)result+1));
             result = (int) (3);
             t3 = (void*) (gc_malloc(&gc_,16));
             *(void**)t3 = (void*)result;
             *((void**)t3+1) = (void*)t2;
             result = (void*) (((FunctionPointer)t1)((void*)t3,gc_));
           }
         }
       }
       gc_stop(&gc_);
       printf("%d\n",(int)result);
       return (int)result;
     }
   }
}
