/* 
  case can contain goto label between case
  case label can be Identifier - enum or const 
*/
int main(int argc, char* argv) {
    switch (argc)
    {
        case 0:
        foo:
        case 1:;
    }

    enum Num {n0,n1,n2,n3,n4};
    enum Num n=n4;
    switch (n) {
        case n0:
        case n1:
        case n2:
            return 0;

        case n3:
        case n4:
            break;
    }
}
