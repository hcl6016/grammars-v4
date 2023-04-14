struct Aa {
    long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
    __extension__ long long;
    long long: 50;
};

int __aligned__; /*__aligned__ is not keyword, __alignof__ is */