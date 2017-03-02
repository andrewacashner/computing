/* minimum.c -- Andrew A. Cashner, 2012/02/02
 * Find the minimum
 */

#include <stdio.h>

int minimum(int *list, int list_length);
int minimum_recursive(int *list, int list_length);
    
int main(void) {
    int list[] = { 3, 2, 43, 7, 22};
    int list_length = 5;
    
    int *list_ptr = list;
    int i;

    for (i = 0; i < list_length; ++i) {
        printf("%d%s", list[i], i < list_length - 1 ? ", " : "\n");
    }

    printf("Minimum: %d\n", minimum(list_ptr, list_length));
    printf("Minimum calculated recursively: %d\n", 
            minimum_recursive(list_ptr, list_length));
    return(0);
}

int minimum(int *list, int list_length) {
    int i, min;
    for (i = 0, min = list[0]; i < list_length; ++i) {
        if (min > list[i]) {
            min = list[i];
        }
    }
    return(min);
}

int minimum_recursive(int *list, int list_length) {
    if (list_length > 0) {
        if (*list > *(list + 1)) {
            return(minimum_recursive(++list, --list_length));
        } 
    }
    return(*list);
}
