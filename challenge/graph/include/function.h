#ifndef FUNCTION_H
#define FUNCTION_H

#include <stdbool.h>

#define FUNCTION_MAX_CHAR_ATOM 20
#define FUNCTION_MAX_CHAR_INPUT 80
#define FUNCTION_MAX_ARGS 10
#define FUNCTION_VAR_ARGS -1

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

Function_sig_ptr lookup_function(char*);
bool Function_sig_is_variadic(Function_sig_ptr);
bool Function_sig_matches_args(Function_sig_ptr, int);

#endif // FUNCTION_H
