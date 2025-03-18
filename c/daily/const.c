#include <stdio.h>

char *alphabet[] = {
    "abc",
    "def"
};

int main(void) {

//    for (int i = 0; i < 2; ++i) {
//        char *sub_alphabet = alphabet[i];
//
////        for (int j = 0; sub_alphabet[j] != '\0'; ++j) {
////            printf("%c", sub_alphabet[j]);
//        for (char *letter_ptr = sub_alphabet; *letter_ptr != '\0'; ++letter_ptr) {
//            printf("%c", *letter_ptr);
//        }
//        printf("\n");
//    }
    for (short letter = 'A'; letter <= 'Z'; ++letter)
    {
        printf("%c", letter);
        if (letter == 'P')
        {
            printf("\n");
        }
    }
    printf("\n");
    return(0);
}
