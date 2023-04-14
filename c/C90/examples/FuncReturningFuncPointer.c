int (*getfn(int param ))(void) {
    return 0;
}

void (*getfn2(int param ))(int, int) {
    return 0;
}

int main()
{
    int (*func)();
    func = getfn(1);
    void (*func2)(int,int);
    func2 = getfn2(2);
}