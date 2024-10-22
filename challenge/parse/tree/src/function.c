/* Call a function by name from a table of functions
 * Andrew Cashner, 2024/10/21
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../include/function.h"

Function_sig function_table[] = {
    {   .name = "add",
        .symbol = "+",
        .function = (generic_function_ptr)add,
        .required_args = FUNCTION_MAX_ARGS
    },
    {
        .name = "subtract",
        .symbol = "-",
        .function = (generic_function_ptr)subtract,
        .required_args = FUNCTION_MAX_ARGS
    },
    {
        .name = "multiply",
        .symbol = "*",
        .function = (generic_function_ptr)multiply,
        .required_args = FUNCTION_MAX_ARGS
    },
    {
        .name = "divide",
        .symbol = "/",
        .function = (generic_function_ptr)divide,
        .required_args = FUNCTION_MAX_ARGS
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
    },
    {
        .name = "FUNCTION_TABLE_END"
    }
};


double evaluate(char *input) {
    char buffer[FUNCTION_MAX_CHAR_INPUT];
    char *buffer_ptr = buffer;
    strcpy(buffer, input);

    if (buffer[0] == '(' && buffer[strlen(buffer) - 1] == ')') {
        buffer[strlen(buffer) - 1] = '\0';
        ++buffer_ptr;
    } else {
        fprintf(stderr, "Unbalanced or missing parentheses\n");
        exit(EXIT_FAILURE);
    }
  
    double args[FUNCTION_MAX_ARGS];
    int arg_count = -1;
    char *input_function_name;

    for (char *token = strtok(buffer_ptr, " ");
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
        lookup_function(input_function_name);

    if (!match_sig) {
        fprintf(stderr, "Unrecognized function %s\n", input_function_name);
        exit(EXIT_FAILURE);
    }
    generic_function_ptr fn = match_sig->function;
    char *name = match_sig->name;
    int required_args = match_sig->required_args;

    // Test arguments
    if (required_args != FUNCTION_MAX_ARGS && arg_count != required_args) {
        fprintf(stderr, "Wrong number of arguments to function %s (%d required, %d supplied)\n", name, required_args, arg_count);
        exit(EXIT_FAILURE);
    }

    // Calculate and report result
    double result = (*fn)(arg_count, args);

    return result;
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

Function_sig_ptr lookup_function(char *symbol) {

    Function_sig_ptr found = NULL;

    if (atof(symbol) == 0) { // Not a number, must be symbol 
        for (int i = 0; 
                strcmp(function_table[i].name, "FUNCTION_TABLE_END") != 0; 
                ++i) {
            if (strcmp(function_table[i].symbol, symbol) == 0) {
                found = &function_table[i];
                break;
            }
        }
    }
    return found;
}

