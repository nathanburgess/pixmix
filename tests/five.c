#include <stdio.h>
void main () {
    int x = 1;
    void bar() {x = x + 3; }
    void foo() {
    int x = 8;
    bar();
    printf("%d\n", x);
   }
    foo ();
}
