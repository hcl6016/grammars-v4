#ifndef _MSC_VER
#define __cdecl
#endif
typedef struct MyType_t{int a,b;} MyType;
typedef struct MyType1_t{int a,b,c;} MyType1;
/*parameter contains pointer*/
int
__cdecl
f1 (
   const MyType        *param1,
   int                  param2
  );


MyType1
__cdecl
f2 (
   MyType        *param1,
   int       *     param2
  );


void
__cdecl
f3 (
   void        *param1
  );