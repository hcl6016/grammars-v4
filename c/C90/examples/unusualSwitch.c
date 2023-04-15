#include <stdio.h>
/*
 loop while(1) will break although c==1 not 2, because in C
 the jump occurs to first case and other cases  are performed despite its values  
*/
void f(){}
int a = 1;
void b() {
    int c = 1;
    for (; c <= 3;) {
        int d = 1;
        switch (c)
            case 1:
                while(1) {
                    printf("");
                    case 2:break;
                }

        switch (c)
        {
            default:
                return;
            case 1:
                if (d)
                    case 2:
                    case 3:
                        f();
        }
        if (a)
            c++;
    }
}

main(int argc, char **argv)
{
    b();
}
