/* Polish notation calculator 
 * Recursive implementation with function pointers
 * Andrew Cashner, 2017/03/23
 */

#include <stdio.h>
#include <stdlib.h>

#define MAX_OPERANDS 12

int add_array(int array[], int i, int max_array, int count);

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
        } else {
            printf(" + %d", operands[i]);
        }
    }
    printf("\n");
    if (argv[1][0] == '+') {
        result = add_array(operands, 0, argc - 1, 0);
    }
    printf("%d\n", result);
    return(0);
}
int add_array(int array[], int i, int max_array, int count)
{   
    int n;
    if (i > max_array) {
        n = 0;
    } else {
        n = array[i];
        printf("+ %d (%d) ", n, count);
        n += add_array(array, i + 1, max_array, count + 1);
    } 
    return(n);
}
