int count_positive(unsigned long[], unsigned long) {
    return 0;
}

int main() {
    unsigned long args[] = {1, 2, 3, 4, 0, 1, -1, 10};
    /*is error  const unsigned long art[] = (const unsigned long[]){1, 2, 3, 4, 0, 1, -1, 10};*/
    count_positive((unsigned long[]){1, 2, 3, 4, 0, 1, -1, 10}, -1);
}