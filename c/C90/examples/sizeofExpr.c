/* argument sizeof can be conditional expresson, must be constant */
int main() {
    const int a=1;
    const int b=2;
    int ss = sizeof(a>b?a:b);
}