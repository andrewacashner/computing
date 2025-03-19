/** 
 * loop through string continuously
 * circular queue of char pointers without using malloc
 * 2025/03/19
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CYCLES 5

typedef struct Node *Node_ptr;
typedef struct Node {
    char *data_ptr;
    Node_ptr next;
} Node;

void fill_pointer_array_from_string(Node_ptr array, char *str);
void circular_print(Node_ptr array, int cycles, int line_length);
void circular_print_str(char *str, int cycles, int line_length);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: circular \"STRING\"\n");
        exit(EXIT_FAILURE);
    }
    char *msg = argv[1];
    int length = strlen(msg);
    Node pointers[length];

    fill_pointer_array_from_string(pointers, msg);
    circular_print(pointers, MAX_CYCLES, length);
    printf("\n");
    
    circular_print(pointers, MAX_CYCLES, length - 1);
    printf("\n");

    circular_print_str(msg, MAX_CYCLES, length);
    printf("\n");
    
    circular_print_str(msg, MAX_CYCLES, length - 1);
    printf("\n");

    return 0;
}

void fill_pointer_array_from_string(Node_ptr array, char *str) {
    int length = strlen(str);

    for (int i = 0; i < length; ++i) {
        array[i].data_ptr = &str[i];
        if (i == length - 1) {
            array[i].next = array;
        } else {
            array[i].next = &array[i + 1];
        }
    }
}

void circular_print(Node_ptr array, int cycles, int line_length) {
    Node_ptr current = array; 
    for (; cycles > 0; --cycles) {
        for (int chars_printed = 0; chars_printed < line_length; ++chars_printed) {
            char this_char = *current->data_ptr;
            printf("%c", this_char);
            current = current->next;
        }
        printf("\n");
    }
}

void circular_print_str(char *str, int cycles, int line_length) {
    int start_index = 0;
    for (; cycles > 0; --cycles) {
        int char_index = start_index;

        for (int chars_printed = 0; chars_printed < line_length; ++chars_printed) {
            char this_char = str[char_index];

            if (this_char == '\0') {
                char_index = 0;
            } 
            printf("%c", this_char);
            ++char_index;
        }

        start_index = char_index;
        printf("\n");
    }
}
