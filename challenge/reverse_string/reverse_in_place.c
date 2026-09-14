/*-----------------------------------------------------------------------------
 * Reverse a string in place
 *
 * Andrew Cashner
 * 2026/09/14
 *-----------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void reverse(char *str);
void reverse_print(char *str);


int main(int argc, char *argv[]) 
{
    if (argc != 2) 
    {
        fprintf(stderr, "Usage: reverse STRING\n");
        exit(EXIT_FAILURE);
    }

    char *input = argv[1];
    printf("\'%s\' -> ", input);

    reverse(input);

    printf("\'%s\'\n", input);

    reverse_print(input);
    return 0;
}

/*-----------------------------------------------------------------------------
 * reverse
 *
 * Reverse a string in place 
 * 
 * INPUT
 *      str : Pointer to character array to reverse
 *
 * OUTPUT
 *      None
 *
 * POSTCONDITION
 *      The string has been reversed.
 *-----------------------------------------------------------------------------*/
void reverse(char *str) 
{
    // O(n)
    int length = strlen(str);

    int start = 0;
    int end = length - 1;
    
    // 3 operations for n/2 positions + n/ 2 increments & compares
    // = 3n
    while (start < end) 
    {
        char tmp = str[start];
        str[start] = str[end];
        str[end] = tmp;
        ++start;
        --end;
    }

    // => O(n)
}

// Just print the string in reverse without changing it
void reverse_print(char *str)
{
    int i = 0;

    // for string of length n (not including '\0')
    // n + 1 reads, compares, index increments
    while (str[i] != '\0') 
    {
        ++i;
    }
  
    // n reads, writes, compares
    while (i >= 0) 
    {
        printf("%c", str[i]);
        --i;
    }

    // total 3 (n + 1) + 3n = 6n + 3 => O(n)

    printf("\n");
}
