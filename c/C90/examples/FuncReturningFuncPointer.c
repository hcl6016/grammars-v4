int (*getfn(int param ))(void) {
    return 0;
}

void (*getfn2(int param ))(int, int) {
    return 0;
}

int main()
{
    int (*func)();
    func = getfn(1);
    void (*func2)(int,int);
    func2 = getfn2(2);
}

/* gcc/gcc/testsuite/gcc.c-torture/compile/pr33641.c */


typedef enum { one, two } exp;
extern exp pe;
extern char pt[256];
void psd (void (*f) (void *), void *p);
static void rle (void *e) { }
void
foo (void)
{
  psd ((void (*)(void *)) (rle), (void *) (pt + pe));
}
