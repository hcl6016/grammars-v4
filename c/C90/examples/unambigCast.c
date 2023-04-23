typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));
typedef float __m512 __attribute__ ((__vector_size__ (64), __may_alias__));
typedef float __v16sf __attribute__ ((__vector_size__ (64)));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));
typedef unsigned short __mmask16;

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_setzero_ps (void)
{
  return __extension__ (__m512){ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
}


__m512 __builtin_ia32_insertf32x4_mask(__v16sf,__v4sf,int , __v16sf, __mmask16);

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_zextpd128_pd512 (__m128d __A)
{
  return (__m512d) ((__m512) __builtin_ia32_insertf32x4_mask ((__v16sf)(__m512) (_mm512_setzero_ps ()), (__v4sf)(__m128) ((__m128) __A), (int) (0), (__v16sf)(__m512) (_mm512_setzero_ps ()), (__mmask16)(-1)));
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_store_ss (float *__P, __m128 __A)
{
  *__P = ((__v4sf)__A)[0];
}


typedef struct matrix {
    int table[5][5];
    unsigned dim;
} matrix_t;

matrix_t matrix1;
/* array of functions returning pointer to struct*/
matrix_t *(*matrices[])(int,int);
int main(void)
{
    matrix_t *matrptr;
    matrptr = matrices[0](1,2);
}