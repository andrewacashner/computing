// 2025/04/08
// Select a function from a menu

#include <stdio.h>

void print_a(void);
void print_b(void);
void print_c(void);
void quit();

typedef void (*void_function_ptr)(void);

const void_function_ptr ACTIONS[] = {
    print_a,
    print_b,
    print_c,
    quit
};

#define MAX_ACTIONS (4)


int main(void) {
    void_function_ptr action = NULL;

    while (action == NULL) {
        printf("MENU:\n1. Print a\n2. Print b\n3. Print c\n4. Quit\n\nEnter your selection: ");

        int input_char, selection_char;
        while ((input_char = getchar()) != '\n') {
            selection_char = input_char;
        }

        int menu_choice = selection_char - '1'; 

        if (menu_choice >= 0 && menu_choice < MAX_ACTIONS) {
            action = ACTIONS[menu_choice];
        } else {
            printf("Unrecognized command; try again\n");
        }
    }

    action();

    return 0;
}

void print_a(void) {
    printf("a\n");
}

void print_b(void) {
    printf("b\n");
}

void print_c(void) {
    printf("c\n");
}

void quit(void) {
    printf("End of program\n");
}
