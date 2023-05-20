int g33(int l31, int l22) {
    // specialization of 0
    return l22; // specialization of 0 (arg 1)
}
int g29(int l27, int l20) {
    // specialization of 0
    return l20; // specialization of 0 (arg 1)
}
int g26(int l24, int l18) {
    // specialization of 0
    return l18; // specialization of 0 (arg 1)
}
int main() {
    // main
    struct{int (*p0)(int,int);}* l17 = {g26}; // specialization of 0
    struct{int (*p0)(int,int);}* l19 = {g29}; // specialization of 0
    int (*l30)(int,int) = l17->p0; // projection into closure tuple (application 6, tuple 17)
    int l42 = (int)l17; // casting the function's closure to an int
    int l43 = (int)l19; // casting the argument closure to an int
    int l44 = l30(l42, l43); // return value before cast closure
    struct{int (*p0)(int,int);}* l6 = (struct{int (*p0)(int,int);}*)l44; // application at "CLI" (line 4, column 9)
    struct{int (*p0)(int,int);}* l21 = {g33}; // specialization of 0
    int (*l34)(int,int) = l21->p0; // projection into closure tuple (application 9, tuple 21)
    int l39 = (int)l21; // casting the function's closure to an int
    int l9 = l34(l39, 3); // application at "CLI" (line 4, column 16)
    int (*l35)(int,int) = l6->p0; // projection into closure tuple (application 5, tuple 6)
    int l36 = (int)l6; // casting the function's closure to an int
    int l5 = l35(l36, l9); // application at "CLI" (line 4, column 12)
    return l5; // application at "CLI" (line 4, column 12)
}
