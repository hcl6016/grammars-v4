void f1() {
    int k = ({
        int result = 0;
        result;
    });
}


void f2() {
    int k =  __extension__({
        int result = 0;
        result;
    });
}
