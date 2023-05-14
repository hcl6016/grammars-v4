#include <stdio.h>
main(int argc, char **argv)
{
    int c = 2;
    switch (c){
        case 0:
        case 1:
            printf("0,1\n");
            break;
        case 2:
            printf("2\n");
            break;
        default:printf(">2\n");
    }
}
