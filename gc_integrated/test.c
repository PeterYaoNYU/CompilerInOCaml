main() {
   let dyenv = 0; {
     let result = 0; {
       let t1 = 0; {
         let t2 = 0; {
           let t3 = 0; {
             let t4 = 0; {
               let t5 = 0; {
                 let t6 = 0; {
                   result = malloc(8);
                   *result = t7;
                   *(result+4) = dyenv;
                   t4 = *result;
                   t5 = *(result+4);
                   result = 42;
                   t6 = malloc(8);
                   *t6 = result;
                   *(t6+4) = t5;
                   result = t4(t6);
                 }
               }
             }
             t1 = *result;
             t2 = *(result+4);
             result = 3;
             t3 = malloc(8);
             *t3 = result;
             *(t3+4) = t2;
             result = t1(t3);
           }
         }
       }
       return result;
     }
   }
}
t7(dyenv) {
   let result = 0; {
     result = malloc(8);
     *result = t8;
     *(result+4) = dyenv;
     return result;
   }
}
t8(dyenv) {
   let result = 0; {
     let t9 = 0; {
       result = **(dyenv+4);
       t9 = result;
       result = *dyenv;
       result = t9+result;
     }
     return result;
   }
}