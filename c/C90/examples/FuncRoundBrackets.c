#include <stdio.h>

char *(func1 (int, int)) {
    return  "abc";
}

char *((func1a (int, int))) {
    return  "abc";
}

char *(((func1b (int, int)))) {
    return  "abc";
}


void (func2(void)) {
    printf("func2\n");
}

void (((func2a(void)))) {
    printf("func2\n");
}

int ((attrFun1 (int, int))) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

int (*(attrFun2 (int, int))) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

int (*(*attrFun3 (int, int))) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern __attribute__((noreturn)) void (voidFunc(int ));

int attrFun4 (int n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

int (attrFun4a (int n)) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
char (*func3 (int, int)) {
    return  "func3";
}


/* this is the same function:  */
void *
__attribute__((__malloc__))
__attribute__((__alloc_size__(2)))
funcXY(int x, int y);

void *
__attribute__((__malloc__))
__attribute__((__alloc_size__(2)))
(funcXY(int x, int y));

void *
__attribute__((__malloc__))
(__attribute__((__alloc_size__(2)))
funcXY(int x, int y));

void *
(__attribute__((__malloc__))
__attribute__((__alloc_size__(2)))
funcXY(int x, int y));

void (*
__attribute__((__malloc__))
__attribute__((__alloc_size__(2)))
funcXY(int x, int y));

int main() {
    printf("%s\n",func1(2,2));
    func2();
    printf("%s\n",func3(2,2));
}