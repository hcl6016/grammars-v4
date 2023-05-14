typedef struct matrix {
    int table[5][5];
    unsigned dim;
} matrix_t;

matrix_t matrix1;
/* array of functions returning pointer to struct*/
matrix_t *(*matrices[])(int,int);

int main(void)
{
    matrix_t *matrptr;
    matrptr = matrices[0](1,2);
}