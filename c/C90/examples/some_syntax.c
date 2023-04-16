#include <stdio.h>
#include <stdlib.h>
/* dirty fragments of syntax */

int *__attribute__((__aligned__(16))) *p;
int b;
int static *estat = &b, *j;
struct __attribute__((packed)) MyStruct { long long a; char b; };
typedef int __attribute__((vector_size (8))) vec;

unsigned long long int * const sssa,cccc;

/* unary expression */
void paramGoto(int param){
    goto *(void*)param;
}

/* postfix expression, arrays */
void arrayExpr ()
{
    char array[100];
    int number;
    int nn = array[number = array[number + 1] ? number + 1 : 0];
    nn = array[number = array[number + 1]];
}


typedef enum aenum{ea,eb,ec=3} newtype4;
/*enum as expression*/
enum EVRPC_HOOK_RESULT {
    EVRPC_TERMINATE = -1,
    EVRPC_CONTINUE = EVRPC_TERMINATE*4,
    EVRPC_PAUSE = 1
};


/* size of tables can be conditional expression */
int pmatch[1>2 ? 2: 3 ];


unsigned long kkko = 0x2U;
int sssgn = - 6;
long double fffg = 5.6L;
void aaa(int pmatch[__restrict]);


/*K&R style*/
int funcKandRstyle(a,b) int a; float b; {
    printf("K&R syntax\n");
    return 0;
}

struct __gconv_step_data{
    int a;
};

typedef struct __gconv_info
{
    int;

    long __nsteps;
    struct __gconv_step *__steps;
    struct __gconv_step_data __data[0];
} *__gconv_t;


struct cifs_sid {
    int revision;
    int  num_subauth;
} uu00, uu001, uu01 __attribute__((packed)) , uu00998;


typedef void (((Z3_error_handler)))(int , int );
char ut_line[32]
        __attribute__ ((__nonstring__)),ut_line1[32]
        __attribute__ ((__nonstring__));
struct ss1
{
    unsigned int wrong_key_usage : 0x3,drugi[2];

    int quot;
    struct ab{
        int f;
    } ;
    int rem;
} vars;

float fun( a, b )
        int a;
        float b;
{
    float c;
    c = a * b;
    return c;
}

struct
{
    int quot;
    int rem;
};

typedef long unsigned int size_t;
typedef int wchar_t;
typedef struct ss
{
    int quot;
    int rem;
    int table[90];
} div_t88;



/*externalDeclaration*/
int glob68;
int glob1 = 1;

struct structure{int field,ff};

extern void funcExtern(int, int k);


void func11(int n, int k) {
    struct structure2{int field;};
    struct structure2 str2;
    str2.field = 2;
    typedef int (*__compar_fn_t) (const void *, const void *p);
}

int func2() {
    return 0;
}
int *func3() {
    int i = 1;
    while (i<10) {
        i++;
    }
    do {
        i--;
    }while(i>0);
    return &glob68;
}

int main() {
    register unsigned int reg __asm ("ecx");
    /* second expression can be omitted, then its value = 1 */
    int k = b ? : 2;

    int c = 22,d;
    printf("%ld", sizeof (int));
    char *str= "\x1";

    /*if expresson can be assignment*/
    if (c=2) c=1;
    if (c==2) c=1; else c=3;

    /* in switch can be comma expression */
    switch(c,d+2) {
        case 1: break;
        default:c=2;
    }
/*can't be b=c=2 in intializer*/
    int a, b = c, *p = NULL;
    int n = a<b? a: a<b?b:a;
    goto lab;
    for (a>2; b>2,a<b; c=2) {}
    printf("Hello, World!\n");
    lab:
    return a=b,1;
}
