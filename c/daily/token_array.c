#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

uint8_t token_count(char *input) {
    uint8_t token_count = 0;
    bool in_word;
    for (int i = 0; input[i] != '\0'; ++i) {
        if (input[i] == ' ') {
            if (in_word) {
                ++token_count;
            }
            in_word = false;
        } else {
            in_word = true;
        }
    }
    if (in_word) {
        ++token_count;
    }

    return token_count;
}

void print_token_labels(char *input) {
    uint8_t num_tokens = token_count(input);
    char labels[num_tokens];

    char label_start = 'a';
    for (uint8_t i = 0; i < num_tokens; ++i) {
        labels[i] = label_start;
        ++label_start;
    }

    printf("[");
    for (uint8_t i = 0; i < num_tokens; ++i) {
        printf("%c", labels[i]);
        if (i < num_tokens - 1) {
            printf(", ");
        }
    }
    printf("]\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: token_array \"STR\"\n");
        exit(EXIT_FAILURE);
    }

    print_token_labels(argv[1]);

    return 0;
}
