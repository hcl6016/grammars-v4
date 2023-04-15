/*
# 2 "/home/andrzej/git/gcc/gcc/testsuite/gcc.target/i386/avx-check.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/11/include/cpuid.h" 1 3 4
# 246 "/usr/lib/gcc/x86_64-linux-gnu/11/include/cpuid.h" 3 4
*/
static __inline unsigned int
__get_cpuid_max (unsigned int __ext, unsigned int *__sig)
{
  unsigned int __eax, __ebx, __ecx, __edx;
# 288 "/usr/lib/gcc/x86_64-linux-gnu/11/include/cpuid.h" 3 4
  __asm__ __volatile__ ("cpuid\n\t" : "=a" (__eax), "=b" (__ebx), "=c" (__ecx), "=d" (__edx) : "0" (__ext));

  if (__sig)
    *__sig = __ebx;

  return __eax;
}


/* gcc/gcc/testsuite/gcc.c-torture/compile/20060217-1.c */

struct U
{
  unsigned int u[256];
};

struct S
{
  int u, v, w, x;
  int s[255];
};

int
foo (struct U *x, struct S *y)
{
  register int i;
  for (i = 0; i < 255; i++)
    {
      unsigned int v;
      __asm__ ("" : "=r" (v) : "0" (x->u[i + 1]) : "cc");
      y->s[i] = v;
    }
  return 0;
}
