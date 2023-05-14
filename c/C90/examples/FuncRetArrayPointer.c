#include <stdio.h>

char tab[3];
char (*func4 (void))[3] {
    tab[0]  = 'a';
    tab[1]  = 'b';
    tab[2]  = 'c';
    return &tab;
}


char (*func5 (void))[2] __attribute__ ((__nothrow__ , __leaf__));

int main() {
    char (*tab)[3] = func4();
    printf("%s %s %s\n",tab[0],tab[1],tab[2]);
}