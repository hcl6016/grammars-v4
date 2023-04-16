const unsigned short int *B = 0;
extern
__typeof (B)
        B1 __asm__ ("?" "" "")
        __attribute__ ((visibility ("hidden")))
        ;

int nn;

extern
__typeof (int *)
        B2 __attribute__ ((alias ("nn" "")));

void f() {
  int a,b,c;
  c = ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a > _b ? _a : _b; });
}

__typeof__ (int *) p1, p2; /* Declares two int pointers p1, p2 */           
int *p1, *p2;             
__typeof__ (int) * p3, p4;/* Declares int pointer p3 and int p4 */             
int * p3, p4;           
__typeof__ (int [10]) a1, a2;/* Declares two arrays of integers */             
int a1[10], a2[10];

long l = (__typeof__(long))1;

int main(){
    return 0;
}