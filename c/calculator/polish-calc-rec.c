/* Polish notation calculator 
 * Recursive implementation with function pointers
 * Andrew Cashner, 2017/03/23
 */

#include <stdio.h>
#include <stdlib.h>

#define MAX_OPERANDS 12
#define END_ARRAY -999

int add_array(int num[], int i, int total);
int add_array_ptr(int *num, int total);

int main(int argc, char *argv[])
{
    int i;
    int operands[MAX_OPERANDS];
    int scantest;
    int result;

    if (argc < 3) {
        exit(EXIT_FAILURE); 
    }
    for (i = 2; i < argc; ++i) {
        scantest = sscanf(argv[i], "%d", &operands[i]);
        if (scantest < 1) {
            exit(EXIT_FAILURE);
        } 
    }
    operands[i] = END_ARRAY;

    if (argv[1][0] == '+') {
/*        result = add_array(operands, argc - 1, 0); */
        result = add_array_ptr(operands, 0);
    }
    printf("%d\n", result);
    return(0);
}

int add_array(int num[], int i, int total)
{
    if (i == 0) {
        return(total);
    } else {
        return(total += num[i] + add_array(num, i - 1, total));
    }
}
int add_array_ptr(int *num, int total)
{
    if ((*num) == END_ARRAY) {
        return(total);
    } else {
        return(total += (*num) + add_array_ptr(num + 1, total));
    }
}
