void (*(*x1) (void)) (float); /* declare x1 as pointer to function (void) returning pointer to function (float) returning void*/
void f1(float); /* function (float) returning void */
void (*x2)(float); /* declare x2 as pointer to function (float) returning void */
void (*f3 (void)) (float); /* function (void) returning pointer to function (float) returning void*/
void (*(f3a) (void)) (float); /* function (void) returning pointer to function (float) returning void*/
int (*x3)(int,int); /* declare x3 as pointer to function (int,int) returning int */
int (f2)(int,int); /* function (int,int) returning int*/
void *f4(float);