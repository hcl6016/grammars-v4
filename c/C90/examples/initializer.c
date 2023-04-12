typedef enum enType {RAX,RBX, RCX, RDX} enTypeDef;
enTypeDef array[2] = {RAX,
                     RDX};

struct MyStr {int field1; int field2;};
struct MyStr var1 = {.field1=1, 2};
