#include <stdlib.h>
#include <stdio.h>

/*extension https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html#Statement-Exprs*/
int foo() {
    return 2;
}


void bar() {
    int r = ({ int y = foo (); int z;
        if (y > 0) z = y;
        else z = - y;
        z; });
}


/* https://gcc.gnu.org/onlinedocs/gcc/Local-Labels.html#Local-Labels*/
int ff2() {
    do{
        __label__ l1,l2;
        l1:;
        l2:;
    }while(0);
    return 0;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html#Labels-as-Values*/
int ff1() {
    foo:;
    void *ptr;
    bar:

    ptr = &&foo;
    hack:
    goto *ptr;
    static void *array[] = { &&foo, &&bar, &&hack };
    goto *array[0];
    static const int array2[] = { &&foo - &&foo, &&bar - &&foo,
    &&hack - &&foo };
    goto *(&&foo + array2[1]);
}

/*https://gcc.gnu.org/onlinedocs/gcc/Nested-Functions.html#Nested-Functions*/
bar2 (int *array, int offset, int size)
{
    __label__ failure;
    int access (int *array, int index)
    {
        if (index > size)
            goto failure;
        return array[index + offset];
    }
    int i;

    for (i = 0; i < size; i++)
         access (array, i);

    return 0;

    /* Control comes here from access
       if it detects an error.*/
    failure:
    return -1;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Typeof.html#Typeof*/
int *x;
__typeof (*x) y;

/*https://gcc.gnu.org/onlinedocs/gcc/Conditionals.html#Conditionals*/

int main() {
    int x = 2,y=3,r;
    r = x ? : y 	;
    r = x ? x : y;
    return 0;
}

/* https://gcc.gnu.org/onlinedocs/gcc/_005f_005fint128.html#g_t_005f_005fint128*/
int main1(){
    __int128 k;
    unsigned __int128 uk;
    signed __int128 sk;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Long-Long.html#Long-Long*/
int main2(){
    long long int k =  1LL;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Complex.html#Complex*/
void f611(){
 _Complex c1;
 __complex__ c2;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Floating-Types.html#Floating-Types*/
void f612(){
  __float128 f1;
  __float80 f2;
}


/*https://gcc.gnu.org/onlinedocs/gcc/Decimal-Float.html#Decimal-Float*/
void f614(){
  _Decimal32 d1;
  _Decimal64 d2;
  _Decimal128 d3;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Hex-Floats.html#Hex-Floats*/
void f615(){
  float d = 0x1.fp3;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html#Zero-Length*/
void f618(){
    struct line {
        int length;
        char contents[0];
    };
    int this_length = 100;
    struct line *thisline = (struct line *)
            malloc (sizeof (struct line) + this_length);
    thisline->length = this_length;
}

/*https://gcc.gnu.org/onlinedocs/gcc/Empty-Structures.html#Empty-Structures*/
struct empty {
};

/*https://gcc.gnu.org/onlinedocs/gcc/Variable-Length.html#Variable-Length*/
void
f620 (int n)
{
    struct S { int x[n]; };
}

/*https://gcc.gnu.org/onlinedocs/gcc/Subscripting.html#Subscripting*/
struct foo1 {int a[4];};

struct foo1 f() {
    struct foo1 res;
    return res;
}

f623 (int index)
{
    return f().a[index];
}

/*https://gcc.gnu.org/onlinedocs/gcc/Compound-Literals.html#Compound-Literals*/

struct foo {int a; char b[2];} structure;
void f628(){
    int x= 2, y=3;
    structure = ((struct foo) {x + y, 'a', 0});
    struct foo temp = {x + y, 'a', 0};
    structure = temp;
    char **foo = (char *[]) { "x", "y", "z" };
    int i = ++(int) { 1 };
    static struct foo X = (struct foo) {1, 'a', 'b'};
    static int Y[] = (int []) {1, 2, 3};
    static int Z[] = (int [3]) {1};
    {
        static struct foo x = {1, 'a', 'b'};
        static int y[] = {1, 2, 3};
        static int z[] = {1, 0, 0};
    }
}

/* https://gcc.gnu.org/onlinedocs/gcc/Designated-Inits.html#Designated-Inits */
void f629(){
    int a[6] = { [4] = 29, [2] = 15 };
    int a1[6] = { 0, 0, 15, 0, 29, 0 };
    int widths[] = { [0 ... 9] = 1, [10 ... 99] = 2, [100] = 3 };
    struct point { int x, y; };
    int xvalue= 7, yvalue = 9;
    struct point p = { .y = yvalue, .x = xvalue };
    struct point p1 = { xvalue, yvalue };
    struct point p2 = { y: yvalue, x: xvalue };
    union foo { int i; double d; };
    union foo f = { .d = 4 };
    int v1=1,v2=2,v4=4;
    int a3[6] = { [1] = v1, v2, [4] = v4 };
    int a4[6] = { 0, v1, v2, 0, v4, 0 };
    int whitespace[256]
            = { [' '] = 1, ['\t'] = 1, ['\h'] = 1,
                    ['\f'] = 1, ['\n'] = 1, ['\r'] = 1 };
    int yv2=2,xv2=4,xv0=6;
    struct point ptarray[10] = { [2].y = yv2, [2].x = xv2, [0].x = xv0 };

}

/*https://gcc.gnu.org/onlinedocs/gcc/Case-Ranges.html#Case-Ranges*/
void f630(){
    int n =3;
    switch(n) {
        case 1+1 ... 5: break;
    }
}

/*https://gcc.gnu.org/onlinedocs/gcc/Cast-to-Union.html#Cast-to-Union*/
void f631(){
    union foo { int i; double d; };
    int x;
    double y;
    union foo z;
    z = (union foo) x;
    z = (union foo) y;
    z = (union foo) { .i = x };
    z = (union foo) { .d = y };
}

/*https://gcc.gnu.org/onlinedocs/gcc/Mixed-Labels-and-Declarations.html#Mixed-Labels-and-Declarations*/
void f632() {
        int i;
        i++;
        int j = i + 2;
};

/*https://gcc.gnu.org/onlinedocs/gcc/Label-Attributes.html#Label-Attributes*/
void f636(){
/* This branch (the fall-through from the asm) is less commonly used */
    ErrorHandling:
            __attribute__((cold, unused)); /* Semi-colon is required here */
    printf("error\n");
    return 0;

    NoError:
    printf("no error\n");

}
/*https://gcc.gnu.org/onlinedocs/gcc/Enumerator-Attributes.html#Enumerator-Attributes*/
enum E {
    oldval __attribute__((deprecated)) = 4,
    newval
};

/*https://gcc.gnu.org/onlinedocs/gcc/Statement-Attributes.html#Statement-Attributes*/
int
foo2 (int x, int y)
{
    __attribute__((assume(x == 42)));
    __attribute__((assume(++y == 43)));
    return x + y;
}
/* https://gcc.gnu.org/onlinedocs/gcc/Character-Escapes.html#Character-Escapes*/
void f643(){
    char *s = "\e";
}