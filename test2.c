int g33(void *l31, int l22) {
    // specialization of 0
    return l22; // specialization of 0 (arg 1)
}
int g29(void *l27, int l20) {
    // specialization of 0
    return l20; // specialization of 0 (arg 1)
}
void* g26(void *l24, void *l18) {
    // specialization of 0
    return l18; // specialization of 0 (arg 1)
}
int main() {
    // main
    struct{void* (*p0)(void*,void*);} *l17 = (&(struct{void* (*p0)(void*,void*);}){g26}); // specialization of 0
    struct{int (*p0)(void*,int);} *l19 = (&(struct{int (*p0)(void*,int);}){g29}); // specialization of 0
    void* (*l30)(void*,void*) = (*l17).p0; // projection into closure tuple (application 6, tuple 17)
    void *l42 = (void*)l17; // casting the function's closure to an int
    void *l43 = (void*)l19; // casting the argument closure to an int
    void *l44 = l30(l42, l43); // return value before cast closure
    struct{int (*p0)(void*,int);} *l6 = (struct{int (*p0)(void*,int);}*)l44; // application at "CLI" (line 3, column 9)
    struct{int (*p0)(void*,int);} *l21 = (&(struct{int (*p0)(void*,int);}){g33}); // specialization of 0
    int (*l34)(void*,int) = (*l21).p0; // projection into closure tuple (application 9, tuple 21)
    void *l39 = (void*)l21; // casting the function's closure to an int
    int l9 = l34(l39, 1); // application at "CLI" (line 3, column 16)
    int (*l35)(void*,int) = (*l6).p0; // projection into closure tuple (application 5, tuple 6)
    void *l36 = (void*)l6; // casting the function's closure to an int
    int l5 = l35(l36, l9); // application at "CLI" (line 3, column 12)
    return l5; // application at "CLI" (line 3, column 12)
}
