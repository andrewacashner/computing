/* Test of strtof
 * Andrew Cashner
 * 2024/10/22
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main(void) {
    char line[80];
    fgets(line, sizeof(line), stdin);

    errno = 0;
    char *conversion_point = line;
    double num = strtof(line, &conversion_point);
    printf("num = %f, errno = %d, conversion_point = %p, string start = %p\n", num, errno, (void *)conversion_point,
            (void *)line);
    if (conversion_point == line) {
        printf("Not a number\n");
    }

    return 0;
}
