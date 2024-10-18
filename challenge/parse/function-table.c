/* Call a function by name from a table of functions
 * Andrew Cashner, 2024/10/18
 */

#include <stdio.h>
#include <stdlib.h>

#define MAX_ARGS 7
#define MAX_CHAR 20

#define FUNCTION_COUNT 4

int add(int, int);
int subtract(int, int);
int multiply(int, int);
int divide(int, int);

typedef struct Function_sig *Function_sig_ptr;

typedef struct Function_sig {
    char name[MAX_CHAR];
    int (*function)(int, int);
    int required_args;
} Function_sig;

Function_sig Function_sig_create(char*, int (*fn)(int, int), int);
Function_sig_ptr lookup_function(Function_sig_ptr table[], char*);

char *function_names[] = {
    "+",
    "-",
    "*",
    "/",
};

int (*function_methods)(int, int)[] = {
    add,
    subtract,
    multiply,
    divide
}

int function_required_args[] = {
    2,
    2, 
    2, 
    2,
};

    
int main(int argc, char *argv[]) {
    // Or can you initialize above with struct literals?
    Function_sig_ptr functions[FUNCTION_COUNT];

    for (int i = 0; i < FUNCTION_COUNT; ++i) {
        Function_sig_ptr this_function = Function_sig_create(
               function_names[i],
               function_methods[i],
               function_required_args[i]);
        Function_sig_functions[i] = this_function;
    }

    // Read arguments into variables
    if (argc <= 1) {
        printf("Usage: function-table FUNCTION ARG1 ARG2 ...\n");
        exit(EXIT_FAILURE);
    }

    char *input_function_name = argv[1];
    char *args[MAX_ARGS];
    int arg_count;

    for (int i = 2, j = 0; 
            i < argc && j < MAX_ARGS - 2; 
            ++i, ++j) {

        args[j] = argv[i];
        ++arg_count;
    }

    // Parse them
    // Compare to fn table
    // TODO but there will be variable numbers of arguments
    // also not just ints
    Function_sig_ptr match_sig = lookup_function(functions, input_function_name);
    if (!found) {
        fprintf(stderr, "Unrecognized function %s\n", name);
        exit(EXIT_FAILURE);
    }
    int (*fn)(int, int) = match_sig->function;
    int required_args = match_sig->required_args;

    // Apply function
    if (arg_count != required_args) {
        fprintf(stderr, "Wrong number of arguments to function %s (%d required, %d supplied)\n", 
                input_function, required_args, arg_count);
        exit(EXIT_FAILURE);
    }

    int result = (*fn)(args[0], args[1]);
    printf("=> %d\n", result);

    for (int i = 0; i < FUNCTION_COUNT; ++i) {
        free(functions[i]);
    }

    return 0;
}

int add(int a, int b) {
    return a + b;
}

int subtract(int a, int b) {
    return a - b;
}

int multiply(int a, int b) {
    return a * b;
}

int divide(int a, int b) {
    return a / b;
}

Function_sig Function_sig_create(char *name, 
        int (*fn)(int, int), int args) {
    Function_sig this = malloc(sizeof(Function_sig));
    strcpy(this->name, name);
    this->function = fn;
    this->required_args = args;
}

Function_sig_ptr lookup_function(Function_sig_ptr table[FUNCTION_COUNT], char *name) {
    Function_sig_ptr found = NULL;

    for (int i = 0; i < FUNCTION_COUNT; ++i) {
        if (strcmp(table[i]->name, name) == 0) {
            found = table[i];
            break;
        }
    }
    return found;
}

