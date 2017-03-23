/* Polish-notation calculator, four functions
 * Using array of function pointers 
 * Andrew Cashner, 2017/03/23
 */

#include <stdio.h>
#include <stdbool.h>

#define MAX_OPERANDS 12

char operator_sign[] = "+-x/";
enum { INC, DEC, MUL, DIV, MAX_OPS } operator_index;

#define MATH_FUNCTION(name, operation)      \
    int name(int array[], int max_array)    \
    {                                       \
        int i;                              \
        int n = array[0];                   \
        for (i = 1; i < max_array; ++i) {   \
            n operation array[i];           \
        }                                   \
        return(n);                          \
    }                                       \

MATH_FUNCTION(inc, +=)
MATH_FUNCTION(dec, -=)
MATH_FUNCTION(mul, *=)
MATH_FUNCTION(div, /=)

int (*operation[4]) (int array[], int max_array) = { inc, dec, mul, div };

int main(int argc, char *argv[])
{
    int i, operator;
    char operator_char;
    int operand[MAX_OPERANDS];
    bool found = false;
    int result, test;

    if (argc < 3 || argc > MAX_OPERANDS) {
        return(1);
    }
    for (i = 0; i < MAX_OPS; ++i) {
        sscanf(argv[1], "%c", &operator_char);
        if (operator_char == (char)operator_sign[i]) {
            operator = i;
            found = true;
            break;
        }
    } if (found == false) {
        return(1);
    }
    for (i = 2; i < argc; ++i) {
        test = sscanf(argv[i], "%d", &operand[i - 2]);
        if (test < 1) {
            return(1);
        }
    }

    result = operation[operator](operand, argc - 2); 
    
    printf("%d\n", result);

    return(0);
}


