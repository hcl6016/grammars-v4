struct sse_t
{
    _Alignas(16)  float sse_data[4];
    __extension__ _Alignas(16)  float ext_data[4];
};

int main(void)
{
    _Alignas(1>2?16:8)  float sse_data[4];
    __extension__ _Alignas(16)  float ext_data[4];
    printf("Alignment of char = %zu\n", _Alignof(char));
    printf("_Alignof(float[10]) = %zu\n", _Alignof(float[10]));
    printf("_Alignof(struct{char c; int n;}) = %zu\n",
           _Alignof(struct {char c; int n;}));
}