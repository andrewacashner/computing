#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LINE_LENGTH 16

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: pad [NUMBER]\n");
        exit(EXIT_FAILURE);
    }
    char *input_num_str = argv[1];
    int input_num = atoi(input_num_str);

    char output_num_str[LINE_LENGTH];
    sprintf(output_num_str, "%d", input_num);

    int len = strlen(output_num_str);
    int pad_width = len + (LINE_LENGTH - len) / 2;
    printf("word len %d, pad len %d\n", len, pad_width);

    printf("'%*d%*s'\n", pad_width, input_num, LINE_LENGTH - pad_width, " ");

    return 0;
}
