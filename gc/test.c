#include <stdlib.h> 
#include <stdio.h>
typedef void* (*FunctionPointer)(void *);
void* main() {
   void* dyenv = 0; {
     void* result = 0; {
       void* t2 = 0; {
         void* t1 = 0; {
           t1 = (void*) (malloc(16));
           result = (int) (4);
           *(void**)t1 = (void*)result;
           result = (int) (5);
           *((void**)t1+1) = (void*)result;
           result = (int*) ((int*)t1);
         }
         result = (void*) ((void **) *((void**)result+1));
         t2 = (int) ((int)result);
         result = (int) (5);
         result = (int) ((int)t2+(int)result);
       }
       printf("%d\n", (int)result);
       return (int)result;
     }
   }
}
