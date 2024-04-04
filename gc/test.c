#include <stdlib.h> 
#include <stdio.h>
typedef void* (*FunctionPointer)(void *);
void* t4(void* dyenv) {
   void* result = 0; {
     void* t5 = 0; {
       result = (void*) ((void **) *(void**)dyenv);
       result = (void*) ((void **) *(void**)result);
       t5 = (int) ((int)result);
       void* t6 = 0; {
         result = (void*) ((void **) *(void**)dyenv);
         result = (void*) ((void **) *((void**)result+1));
         result = (void*) ((void **) *(void**)result);
         t6 = (int) ((int)result);
         result = (void*) ((void **) *(void**)dyenv);
         result = (void*) ((void **) *((void**)result+1));
         result = (void*) ((void **) *((void**)result+1));
         result = (void*) ((void **) *(void**)result);
         result = (int) ((int)t6+(int)result);
       }
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
             void* t7 = 0; {
               t7 = (void*) (malloc(16));
               result = (int) (1);
               *(void**)t7 = (void*)result;
               void* t8 = 0; {
                 t8 = (void*) (malloc(16));
                 result = (int) (2);
                 *(void**)t8 = (void*)result;
                 void* t9 = 0; {
                   t9 = (void*) (malloc(16));
                   result = (int) (3);
                   *(void**)t9 = (void*)result;
                   void* t10 = 0; {
                     t10 = (void*) (malloc(16));
                     result = (int) (4);
                     *(void**)t10 = (void*)result;
                     result = (int) (5);
                     *((void**)t10+1) = (void*)result;
                     result = (int*) ((int*)t10);
                   }
                   *((void**)t9+1) = (void*)result;
                   result = (int*) ((int*)t9);
                 }
                 *((void**)t8+1) = (void*)result;
                 result = (int*) ((int*)t8);
               }
               *((void**)t7+1) = (void*)result;
               result = (int*) ((int*)t7);
             }
             t3 = (void*) (malloc(16));
             *(void**)t3 = (void*)result;
             *((void**)t3+1) = (void*)t2;
             result = (void*) (((FunctionPointer)t1)((void*)t3));
           }
         }
       }
       return (int)result;
     }
   }
}
