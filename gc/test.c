#include <stdlib.h> 
#include <stdio.h>
typedef void* (*FunctionPointer)(void *);
void* t4(void* dyenv) {
   void* result = 0; {
     void* t5 = 0; {
       result = (void*) ((void **) *(void**)dyenv);
       result = (int) ((void **) *(int*)result);
       t5 = (int) ((int)result);
       result = (void*) ((void **) *(void**)dyenv);
       result = (int) ((void **) *((int*)result+1));
       result = (int) ((int)t5+(int)result);
     }
     return (void*)result;
   }
}
void* main() {
   void* dyenv = 0; {
     void* result = 0; {
       void* t1 = 0; {
         void* t2 = 0; {
           void* t3 = 0; {
             result = (void*) (malloc(16));
             *(FunctionPointer*)result = ((FunctionPointer)t4);
             *((void**)result+1) = (void*)dyenv;
             t1 = (FunctionPointer) ((void **) *(void**)result);
             t2 = (void*) ((void **) *((void**)result+1));
             void* t6 = 0; {
               t6 = (void*) (malloc(8));
               result = (int) (4);
               *(int*)t6 = (int)result;
               result = (int) (5);
               *((int*)t6+1) = (int)result;
               result = (int*) ((int*)t6);
             }
             t3 = (void*) (malloc(16));
             *(void**)t3 = (void*)result;
             *((void**)t3+1) = (void*)t2;
             result = (void*) (((FunctionPointer)t1)((void*)t3));
           }
         }
       }
       printf("%d\n", (int)result);
       return (int)result;
     }
   }
}