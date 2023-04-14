#include <stdio.h>

    int main()
    {
        int i, sum = 0,LAST = 10;
       
        for ( i = 1; i <= LAST; i++ ) {
          sum += i;
        } /*-for-*/
        printf("sum = %d\n", sum);

        return 0;
    }

