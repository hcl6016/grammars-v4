typedef char CustomType;
int a1,a5;
CustomType a2;
CustomType *a3;
CustomType **a4;
int b1,b2,b3,b4;
int b5(){};
f()
{
	a1 = (int)(b1);
	a2 = (CustomType)(b2);
	a3 = (CustomType *)(b3);
	a4 = (CustomType **)(b4);
	a5 = b5();
}
