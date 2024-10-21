/* Call a function by name from a table of functions
 * Andrew Cashner, 2024/10/18
 *
 * TODO
 * not just ints?
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_CHAR_ATOM 20
#define MAX_CHAR_INPUT 80
#define MAX_ARGS 10
#define INF MAX_ARGS
#define FUNCTION_COUNT 6

typedef double (*generic_function_ptr)(int, double[]);

double apply(double (*fn)(double, double), int argc, double argv[]);
double add(int, double[]);
double subtract(int, double[]);
double multiply(int, double[]);
double divide(int, double[]);
double absolute_value(int, double[]);
double exponent(int, double[]);


typedef struct Function_sig *Function_sig_ptr;
typedef struct Function_sig {
    char name[MAX_CHAR_ATOM];
    char symbol[MAX_CHAR_ATOM];
    generic_function_ptr function;
    int required_args;
} Function_sig;

Function_sig_ptr lookup_function(Function_sig[], char*);

Function_sig _function_table[] = {
    {   .name = "add",
        .symbol = "+",
        .function = (generic_function_ptr)add,
        .required_args = MAX_ARGS
    },
    {
        .name = "subtract",
        .symbol = "-",
        .function = (generic_function_ptr)subtract,
        .required_args = MAX_ARGS
    },
    {
        .name = "multiply",
        .symbol = "*",
        .function = (generic_function_ptr)multiply,
        .required_args = MAX_ARGS
    },
    {
        .name = "divide",
        .symbol = "/",
        .function = (generic_function_ptr)divide,
        .required_args = MAX_ARGS
    },
    {
        .name = "absolute value",
        .symbol = "abs",
        .function = (generic_function_ptr)absolute_value,
        .required_args = 1
    },
    {
        .name = "exponent",
        .symbol = "expt",
        .function = (generic_function_ptr)exponent,
        .required_args = 2
    }
};

int main(void) {
    char buffer[MAX_CHAR_INPUT];
    fgets(buffer, sizeof(buffer), stdin);
    buffer[strlen(buffer) - 1] = '\0';
    char *input = buffer;

    if (input[0] == '(' && input[strlen(input) - 1] == ')') {
        input[strlen(input) - 1] = '\0';
        ++input;
    } else {
        fprintf(stderr, "Expression must be enclosed in parentheses\n");
        exit(EXIT_FAILURE);
    }
  
    double args[MAX_ARGS];
    int arg_count = -1;
    char *input_function_name;

    for (char *token = strtok(input, " ");
            token;
            token = strtok(NULL, " ")) {

        if (arg_count < 0) {
            input_function_name = token;
        } else {
            args[arg_count] = atof(token);
        }
        ++arg_count;
    }

    // Parse them
    // Compare to fn table
    Function_sig_ptr match_sig = 
        lookup_function(_function_table, input_function_name);

    if (!match_sig) {
        fprintf(stderr, "Unrecognized function %s\n", input_function_name);
        exit(EXIT_FAILURE);
    }
    generic_function_ptr fn = match_sig->function;
    char *name = match_sig->name;
    int required_args = match_sig->required_args;

    // Test arguments
    if (required_args != INF && arg_count != required_args) {
        fprintf(stderr, "Wrong number of arguments to function %s (%d required, %d supplied)\n", name, required_args, arg_count);
        exit(EXIT_FAILURE);
    }

    printf("Apply function %s to %d arguments: ", name, required_args);

    for (int i = 0; i < arg_count; ++i) {
        printf("%f ", args[i]);
    }
    printf("\n");

    // Calculate and report result
    double result = (*fn)(arg_count, args);
    printf("=> %f\n", result);

    return 0;
}

double apply(double (*fn)(double, double), int argc, double argv[]) {
    double result = argv[0];
    for (int i = 1; i < argc; ++i) {
        result = fn(result, argv[i]);
    }
    return result;
}

double do_add(double n, double m) {
    return n + m;
}

double add(int argc, double argv[]) {
    return apply(do_add, argc, argv);
}

double do_subtract(double n, double m) {
    return n - m;
}

double subtract(int argc, double argv[]) {
    return apply(do_subtract, argc, argv);
}

double do_multiply(double n, double m) {
    return n * m;
}

double multiply(int argc, double argv[]) {
    return apply(do_multiply, argc, argv);
}

double do_divide(double n, double m) {
    return n / m;
}

double divide(int argc, double argv[]) {
    return apply(do_divide, argc, argv);
}


double absolute_value(int argc, double argv[]) {
    return abs(argv[0]);
}

double exponent(int argc, double argv[]) {
    return pow(argv[0], argv[1]);
}

Function_sig_ptr lookup_function(Function_sig table[], char *symbol) {

    Function_sig_ptr found = NULL;

    for (int i = 0; i < FUNCTION_COUNT; ++i) {
        if (strcmp(table[i].symbol, symbol) == 0) {
            found = &table[i];
            break;
        }
    }
    return found;
}

