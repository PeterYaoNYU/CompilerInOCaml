#include<stdio.h>
#include<stdlib.h>

int main(){

    int **a = malloc(10*sizeof(int*));
    int i;
    for(i = 0;i<10;i++){
        a[i] = malloc(4*sizeof(int));
    }
    printf("hello world!");
    free(a);
    int **b = malloc(10*sizeof(int*));
    for(i = 0;i<10;i++){
        b[i] = malloc(4*sizeof(int));
    }
    return 0;
}