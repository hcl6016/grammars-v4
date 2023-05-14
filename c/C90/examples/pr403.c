#include <stdio.h>
char *_(char *s) {
    return s;
}
/* Hello World program */
char *program_name = "program";
int main()
{
      printf (_("\
Usage: %s [ignored command line arguments]\n\
  or:  %s OPTION\n\
"),
          program_name, program_name);

}

