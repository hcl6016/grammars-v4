/*variables*/
int arr[];
int *arr1[];
int *x;
int ((x1));
int *(x2);
int (*(x3));
void (*(*x4) (void)) (float); /* declare x1 as pointer to function (void) returning pointer to function (float) returning void*/
void (*x5)(float); /* declare x2 as pointer to function (float) returning void */
int (*x6)(int,int); /* declare x3 as pointer to function (int,int) returning int */
void *(*x7) (void);
int a0[];
int *a1[];

/*functions*/
int f0();
int *f1();
void *f2(float); /* simple function returns pointer to void*/
void f3(float); /* function (float) returning void */
void (*f4 (void)) (float); /* function (void) returning pointer to function (float) returning void*/
void (*(f5) (void)) (float); /* function (void) returning pointer to function (float) returning void*/
int (f6)(int,int); /* function (int,int) returning int*/

char *(func1 (int, int)) ;
char *((func1a (int, int))) ;
char *(((func1b (int, int)))) ;
void (func2(void)) ;
void (((func2a(void)))) ;
int ((attrFun1 (int, int))) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
int (*(attrFun2 (int, int))) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
int (*(*attrFun3 (int, int))) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
extern __attribute__((noreturn)) void (voidFunc(int ));
int attrFun4 (int n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
int (attrFun4a (int n)) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
char *func3 (int, int);
void *funcXY(int, int);
void *(funcXY(int, int));
void (*funcXY(int, int));
char (*func4 (void))[] ;
char (*func5 (void))[] ;
