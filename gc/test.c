#include <stdlib.h> 
typedef void* (*FunctionPointer)(void);
void* t8(void* dyenv) {
   void* result = 0; {
     void* t9 = 0; {
       result = (int) (**((void*)dyenv+4));
       t9 = (int) ((int)result);
       result = (int) (*(void*)dyenv);
       result = (int) ((int)t9+(int)result);
     }
     return (void*)result;
   }
}
void* t7(void* dyenv) {
   void* result = 0; {
     result = (void*) (malloc(8));
     *((FunctionPointer)result) = ((FunctionPointer)t8);
     *((int)result+4) = (void*)dyenv;
     return (void*)result;
   }
}
void* main() {
   void* dyenv = 0; {
     void* result = 0; {
       void* t1 = 0; {
         void* t2 = 0; {
           void* t3 = 0; {
             void* t4 = 0; {
               void* t5 = 0; {
                 void* t6 = 0; {
                   result = (void*) (malloc(8));
                   *((FunctionPointer)result) = ((FunctionPointer)t7);
                   *((int)result+4) = (void*)dyenv;
                   t4 = (FunctionPointer) (*(void*)result);
                   t5 = (void*) (*((void*)result+4));
                   result = (int) (42);
                   t6 = (void*) (malloc(8));
                   *(int*)t6 = (int)result;
                   *((int)t6+4) = (void*)t5;
                   result = (void*) (((FunctionPointer)t4)((void*)t6));
                 }
               }
             }
             t1 = (FunctionPointer) (*(void*)result);
             t2 = (void*) (*((void*)result+4));
             result = (int) (3);
             t3 = (void*) (malloc(8));
             *(int*)t3 = (int)result;
             *((int)t3+4) = (void*)t2;
             result = (void*) (((FunctionPointer)t1)((void*)t3));
           }
         }
       }
       return (int)result;
     }
   }
}