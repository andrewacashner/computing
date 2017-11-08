/* Find first missing positive integer in unsorted series */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

int first_missing(int array[], int size);

int main(void) {

    int array[] = { -2, 2, -3, 3, -6, 1, 9, 0, -1, 4, 5, 7 };
   
    printf("%d\n", (first_missing(array, 12)));
    
    return(0);
}

int first_missing(int array[], int size) {
    int i;
    int *result = malloc(size*sizeof(int));
    memset(result, false, size);

    for (i = 0; i < size; ++i) {
        if (array[i] > 0) {
            /* Mark result array indexes that match positive integers */
            result[array[i]] = true;
        }
    }
    for (i = 1; i < size; ++i) {
        /* Find first false result index */
        if (result[i] == false) {
            break;
        }
    }
    return(i);
}

