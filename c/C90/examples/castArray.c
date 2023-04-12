int count_positive(unsigned long[], unsigned long) {
    return 0;
}

int main() {
    unsigned long args[] = {1, 2, 3, 4, 0, 1, -1, 10};
    unsigned long art[] = (unsigned long[]){1, 2, 3, 4, 0, 1, -1, 10};
    count_positive((unsigned long[]){1, 2, 3, 4, 0, 1, -1, 10}, -1);
}