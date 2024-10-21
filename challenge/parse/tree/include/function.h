#ifndef FUNCTION_H
#define FUNCTION_H

#define FUNCTION_MAX_CHAR_ATOM 20
#define FUNCTION_MAX_CHAR_INPUT 80
#define FUNCTION_MAX_ARGS 10

double evaluate(char *input);

double apply(double (*fn)(double, double), int argc, double argv[]);

double add(int, double[]);
double subtract(int, double[]);
double multiply(int, double[]);
double divide(int, double[]);
double absolute_value(int, double[]);
double exponent(int, double[]);

typedef double (*generic_function_ptr)(int, double[]);

typedef struct Function_sig *Function_sig_ptr;
typedef struct Function_sig {
    char name[FUNCTION_MAX_CHAR_ATOM];
    char symbol[FUNCTION_MAX_CHAR_ATOM];
    generic_function_ptr function;
    int required_args;
} Function_sig;

Function_sig_ptr lookup_function(Function_sig[], char*);

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

#endif // FUNCTION_H
