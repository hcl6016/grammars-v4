struct Struct1 {
    char a;
    long b;
    short c[3];
    double d;
    char e[21];
    float f;
    struct {
        int a;
        char b;
        long c;
    } g;
    int x[];
};


int main(void)
{
    struct Struct1 var;
    int ret = __builtin_offsetof (struct Struct1,c);
    printf("%d\n",ret);
    ret = __builtin_offsetof (struct aa{int a,c},c);
    printf("%d\n",ret);
}
