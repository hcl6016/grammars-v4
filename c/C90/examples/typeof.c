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

int main(){
    return 0;
}