typedef enum enType {RAX,RBX, RCX, RDX} enTypeDef;
enTypeDef array[2] = {RAX,
                     RDX};

struct MyStr {int field1; int field2;};
struct MyStr var1 = {.field1=1, 2};

void f() {
    int a;
    register reg = a = 5;

    int arr[10] = {[1 > 2 ? 2 : 3] = reg = 2, [7] = 6};
    int arr1[10] = {1, 2, reg = 3};
}
