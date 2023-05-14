#include <stdio.h>

void fn1 ( char *f )
{
    static const void *const step0_jumps[] = { &&do_form_unknown, &&do_flag_plus, &&do_form_float };
    const void * ptr = step0_jumps[0];
    do {
        char spec;
        spec = (*++f);
        goto *ptr;
        do_flag_plus:
        printf("1");
        do_number:
        printf("2");
        do_form_float:
        if (ptr != ((void *)0))
        {
            spec = 'x';
            goto do_number;
        }
        if (spec != 'S')
            printf("3");
        return;
        do_form_unknown:;
    }
    while (*f != '\0');
}

int main(int argc, char **argv)
{
    __label__ lbl1;
    __label__ lbl2;
    void* ptr = &&lbl1;
lbl1:
    return 1;
lbl2:
    return 2;

}


/* gcc/gcc/testsuite/gcc.c-torture/compile/pr21356.c */

int a;
void* p;

void foo (void)
{
  switch (a)
  {
    a0: case 0: p = &&a1;
    a1: case 1: p = &&a2;
    a2: default: p = &&a1;
  }
  goto *p;
}
