/* argument of sizeof can be conditional expression, must be constant */
int main() {
    const int a=1;
    const int b=2;
    int ss = sizeof(a>b?a:b);
}