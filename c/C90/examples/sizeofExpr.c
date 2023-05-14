/* argument of sizeof can be conditional expression, must be constant */
int main() {
    const int a=1;
    const int b=2;
    int ss = sizeof(a>b?a:b);
    int arrSize1 = sizeof(int[100]);
    int arrSize2 = sizeof(int(*[100]));
}