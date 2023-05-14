typedef unsigned int UINTN;
typedef struct MyStruct_t {int a,b;} MyStruct;

/*function returns pointer*/
void *
f1(
   UINTN             param1
  );


int *
f2 (
   int             param1
  );


MyStruct *
f3 (
   int             param1,
   char            param2
  );
