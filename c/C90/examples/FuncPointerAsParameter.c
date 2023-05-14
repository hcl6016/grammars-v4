/* function bar has as parameter function pointer which cas one parameter int a returns int*/
void bar (int param(int)) {
    int (*n) (int) = param;
}

/*K&R style */
int bar1(param) int param(int);{
    return 0;
}


/* where foo is unnecessary parameter name */
void bar (int param(int foo));

/* in declaration parameter name may be omitted */
void bar (int (int foo));

/* can have attributes
(from gcc/gcc/testsuite/gcc.c-torture/compile/20010313-1.c) */
void bar (int (__attribute__((__mode__(__SI__))) int foo));

int func(int foo1) {
    return 0;
}

int main(int argc, char **argv)
{
    bar(func);
}
