#include <stdio.h>
#include <stdlib.h>

int count_letter(char model, char this_char, int count) {
    if (this_char == model) {
        ++count;
    }
    return(count);
}

const char* usage = "Usage: lettercount LETTER\n"
"  The program counts occurrences of LETTER in the text supplied\n"
"  by standard input (case-sensitive)\n";

int main(int argc, char *argv[]) {
    char letter, this_char;
    int count = 0;

    if (argc != 2) {
        printf("%s", usage);
        exit(1);
    } else {
        letter = argv[1][0];
    }

    do {
        this_char = getchar();
        count = count_letter(letter, this_char, count);
    } while (this_char != EOF);
    
    printf("%d\n", count);
    return(0);
}
