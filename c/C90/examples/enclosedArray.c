#include <stdio.h>

int bar2(int *([]));

int bar2(int *(param[])) {
    int n = **param;
    return 0;
}

int bar2KR(param) int *(param[]); {
    int n = **param;
    return 0;
}

/* distinguish from */
int (*getfn(int param ))(void) {
    return 0;
}

void (*getfn2(int param ))(int, int) {
    return 0;
}

void (*getfn3(int param ))(int[]) {
    return 0;
}

int main(int argc, char **argv)
{
    /* 
	compiler error 
    printf("sizeof=%d",sizeof(int *[]));
    printf("sizeof=%d",sizeof(int *([])));*/
    printf("sizeof=%d",sizeof(int *[2]));
    printf("sizeof=%d",sizeof(int *([2])));
    printf("sizeof=%d",sizeof(int (*)[]));
    /*printf("sizeof=%d",sizeof(int (*)([]))); compile error */
}