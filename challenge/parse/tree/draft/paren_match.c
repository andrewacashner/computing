#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#define MAX_CHAR 80

bool do_parentheses_match(char*);

int main(void) {
    char buffer[MAX_CHAR];
    fgets(buffer, sizeof(buffer), stdin);
    buffer[strlen(buffer) - 1] = '\0';
    
    char *response = do_parentheses_match(buffer) ? "Valid" : "Invalid";
    printf("%s\n", response);

    return 0;
}

#define MAX_LEVEL 100

bool do_parentheses_match(char *expr) {
    bool is_valid = true;
    int level = 0;
    int max_level = 0;
    typedef enum { UNSET, OPENED, CLOSED } status;
    status pair[MAX_LEVEL] = { UNSET };
   
    if (expr[0] != '(' || expr[strlen(expr) - 1] != ')') {
        return false;
    }

    for (int i = 0; expr[i] != '\0'; ++i) {
        switch (expr[i]) {
            case '(':
                printf("Found open paren at level %d\n", level);
                pair[level] = OPENED;
                ++level;
                ++max_level;
                break;
            case ')':
                printf("Found close paren: testing level %d\n", level - 1);
                if (level == 0) {
                    pair[level] = UNSET;
                } else if (pair[level - 1] == OPENED) {
                    --level;
                    pair[level] = CLOSED;
                } 
                break;
            default:
                break;
        }
    }
   
    for (int i = 0; i < max_level && is_valid; ++i) {
        char *status;
        switch (pair[i]) {
            case OPENED:
                status = "opened";
                break;
            case CLOSED:
                status = "closed";
                break;
            default:
                status = "unset";
                break;
        }
        printf("%d: %s\n", i, status);

        is_valid = pair[i] == CLOSED;
    }

    return is_valid;
}
