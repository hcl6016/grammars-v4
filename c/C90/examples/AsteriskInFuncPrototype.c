/* see https://stackoverflow.com/questions/17371645/why-use-an-asterisk-instead-of-an-integer-for-a-vla-array-parameter-of-a-f*/

void run_callbacks(int [*], char [*], double (*)[*]);

void run_callbacks(int param1[*], char param2[*], double (*param3)[*]);

void run_callbacks(int param1[2], char param2[5], double (*param3)[3]) {}

