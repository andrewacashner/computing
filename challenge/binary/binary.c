#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_DIGITS 40

int main(int argc, char *argv[]) {
    int test, input_int, i, j, value, current_int;
    char *integer_string;
    int binary_values[MAX_DIGITS];
    char binary_string[MAX_DIGITS];
    double place_values[MAX_DIGITS];
    div_t result; 

    if (argc != 2) {
        fprintf(stderr, "Usage: binary INTEGER\n");
        exit(EXIT_FAILURE);
    }
    
    integer_string = argv[1];
    test = sscanf(integer_string, "%d", &input_int);

    if (test != 1) {
        fprintf(stderr, "Could not read argument %s\n", integer_string);
        exit(EXIT_FAILURE);
    }

    i = 0;
    current_int = input_int; 
    do {
        result = div(current_int, 2);
        current_int = result.quot;
        binary_values[i] = result.rem;
        ++i;
    } while (result.quot > 0);

    for (j = i - 1; j >= 0; --j) {
        binary_string[i - j - 1] = binary_values[j] == 1 ? '1' : '0';
    }
    binary_string[i] = '\0';

/*
    for (i = 0; i <= MAX_DIGITS; ++i) {
        binary_string[i] = '0';
        value = pow(2.0, (double)i);
        if (value <= input_int) {
            place_values[i] = value;
        } else break;
    }
    if (i == 0) ++i;
    binary_string[i] = '\0';

    current_int = input_int;
    for (j = i - 1; j >= 0 && current_int > 0; --j) {
        if (place_values[j] <= current_int) {
            binary_string[i - j - 1] = '1';
            current_int -= place_values[j];
        }
    }
*/
    printf("%s\n", binary_string);

    return(0);
}



