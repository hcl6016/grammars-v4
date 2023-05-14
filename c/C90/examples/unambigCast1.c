typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_move_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_shuffle ((__v4sf)__A, (__v4sf)__B,
                                     __extension__
                                     (__attribute__((__vector_size__ (16))) int)
                                     {4,1,2,3});
}