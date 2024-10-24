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
    int i, t, n;
    int *result = NULL;
   
    /* Find maximum value in array and create result array of that size */
    for (i = t = 0; i < size; ++i) {
        if (array[i] > t) {
            t = array[i];
        }
    }
    result = malloc(t*sizeof(int));
    memset(result, false, t);

    /* Mark result array indexes that match positive integers */
    for (i = 0; i < size; ++i) {
        if (array[i] > 0) {
            n = array[i];
            result[n] = true;
        }
    }
    
    /* Find first false result index */
    for (i = 1; i < t; ++i) {
        if (result[i] == false) {
            break;
        }
    }
    return(i);
}

