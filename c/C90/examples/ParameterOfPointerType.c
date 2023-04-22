typedef struct MyType_t{int a,b;} MyType;
typedef struct MyType1_t{int a,b,c;} MyType1;
/*parameter contains pointer*/
int
f1 (
   const MyType        *param1,
   int                  param2
  );


MyType1
f2 (
   MyType        *param1,
   int       *     param2
  );


void
f3 (
   void        *param1
  );