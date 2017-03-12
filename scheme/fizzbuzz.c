/* FizzBuzz, Andrew Cashner, 2017/03/12 
 * Print numbers 1 to 100; if divisible by 3, print "Fizz" instead; if by 5,
 * "Buzz", if by 15, "FizzBuzz".
 */

#include <stdio.h>

int main(void) {
    int i;
    for (i = 0; i <= 100; ++i) {
        if (i % 15 == 0) {
            printf("FizzBuzz\n");
        } else if (i % 3 == 0) {
            printf("Fizz\n");
        } else if (i % 5 == 0) {
            printf("Buzz\n");
        } else {
            printf("%d\n", i);
        }
    }
    return(0);
}
