/* function parameters */

void f(void (*(*) (void)) (float), void (*)(float), int (*)(int,int));

void f(void (*(*p1) (void)) (float), void (*p2)(float), int (*p3)(int,int)) {
    void (*(*x1) (void)) (float); /* declare x1 as pointer to function (void) returning pointer to function (float) returning void*/
    void (*x2)(float); /* declare x2 as pointer to function (float) returning void */
    int (*x3)(int,int); /* declare x3 as pointer to function (int,int) returning int */
    x1 = p1;
    x2 = p2;
    x3 = p3;
}

void fKR(x1,x2,x3)
        void (*(*x1) (void)) (float); /* declare x1 as pointer to function (void) returning pointer to function (float) returning void*/
        void (*x2)(float); /* declare x2 as pointer to function (float) returning void */
        int (*x3)(int,int); /* declare x3 as pointer to function (int,int) returning int */{
}

/* function returning types */
void f1(float); /* function (float) returning void */
int (f2)(int,int); /* function (int,int) returning int*/

void (*f3 (void)) (float), /* function (void) returning pointer to function (float) returning void*/
     (*(f3a) (void)) (float), /* function (void) returning pointer to function (float) returning void*/
     *f4(float); /*function returning pointer to void*/

void f1(float p1) {}
int (f2)(int p1,int p2) {return 0;}
void (*f3 (void)) (float) {return 0;}
void (*(f3a) (void)) (float){return 0;}
void *f4(float p1){return 0;}

/* K&R style*/
void f1KR(p1)float p1; {}
int (f2KR)(p1,p2) int p1;int p2; {return 0;}
void (*f3KR (p)) (float) int p; {return 0;}
void (*(f3aKR) (p)) (float) int p;{return 0;}
void *f4KR(p1)float p1;{return 0;}

int main(){

}