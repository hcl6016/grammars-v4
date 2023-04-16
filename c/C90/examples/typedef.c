typedef void (*(*x1) (void)) (float); /* x1 is pointer to function (void) returning pointer to function (float) returning void*/
typedef void (*x2)(float); /* x2 is pointer to function (float) returning void */
typedef int (*x3)(int,int); /* x3 is pointer to function (int,int) returning int */
