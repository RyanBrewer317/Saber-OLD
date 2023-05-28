int g40(void *l38, int l29) {
    // specialization of 5
    return l29; // specialization of 5 (arg 6)
}
int g36(void *l34, int l27) {
    // specialization of 5
    return l27; // specialization of 5 (arg 6)
}
void* g33(void *l31, void *l25) {
    // specialization of 5
    return l25; // specialization of 5 (arg 6)
}
int main() {
    // main
    struct{void* (*p0)(void*,void*);} *l24 = (&(struct{void* (*p0)(void*,void*);}){g33}); // specialization of 5
    struct{int (*p0)(void*,int);} *l26 = (&(struct{int (*p0)(void*,int);}){g36}); // specialization of 5
    void* (*l37)(void*,void*) = (*l24).p0; // projection into closure tuple (application 11, tuple 24)
    void *l49 = (void*)l24; // casting the function's closure to an int
    void *l50 = (void*)l26; // casting the argument closure to an int
    void *l51 = l37(l49, l50); // return value before cast closure
    struct{int (*p0)(void*,int);} *l11 = (struct{int (*p0)(void*,int);}*)l51; // application at "CLI" (line 6, column 9)
    struct{int (*p0)(void*,int);} *l28 = (&(struct{int (*p0)(void*,int);}){g40}); // specialization of 5
    int (*l41)(void*,int) = (*l28).p0; // projection into closure tuple (application 14, tuple 28)
    void *l46 = (void*)l28; // casting the function's closure to an int
    int l14 = l41(l46, 1); // application at "CLI" (line 6, column 16)
    int (*l42)(void*,int) = (*l11).p0; // projection into closure tuple (application 10, tuple 11)
    void *l43 = (void*)l11; // casting the function's closure to an int
    int l10 = l42(l43, l14); // application at "CLI" (line 6, column 12)
    return l10; // application at "CLI" (line 6, column 12)
}
