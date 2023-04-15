#include <stdio.h>
typedef unsigned char boolean;

void Sort_array(Tab,Last) int Tab[]; int Last; {
   boolean Swap;
   int Temp,I;
   do {
      Swap = 0;
      for (I = 0; I<Last; I++)
  if (Tab[I] > Tab[I+1]) {
     Temp = Tab[I];
     Tab[I] = Tab[I+1];
     Tab[I+1] = Temp;
     Swap = 1;
     }
      }
   while (Swap);
}

int bar1(param) int __attribute__(()) param(void); {
    return 0;
}

int funcKandRstyle(a,b) int a; float b; {
   printf("K&R syntax\n");
   return 0;
}

