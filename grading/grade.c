#include <stdio.h>
#include <stdlib.h>
#include <math.h>

const enum { l_E, l_D, l_C, l_B, l_A } letter_index;
const enum { q_plus, q_minus, q_pure } quality_index;
const char letters[] = "EDCBA";
const char qualities[] = "+-";

int main(int argc, char *argv[]) {
    double orig_points;
    long int points;
    div_t div_ten;
    char letter, quality;
    int letter_i, quality_i;

    /* Check input and convert string to double */
    if (argc != 2) {
        exit(EXIT_FAILURE);
    }
    if (sscanf(argv[1], "%lf", &orig_points) != 1) {
        exit (EXIT_FAILURE);
    } else {
       
        /* Round points to nearest integer */
        points = lround(orig_points);

        /* Screen out A+ and E grades */
        if (points >= 97) {
            letter_i = l_A;
            quality_i = q_plus;
        } else if (points < 60) {
            letter_i = l_E;
            quality_i = q_pure;
        } else {
            /* Calculate letter based on quotient over 10 */
            div_ten = div(points, 10);
            letter_i = div_ten.quot - 5;

            /* Calculate quality based on range of remainder over 10 */
            if (div_ten.rem >= 7) {
                quality_i = q_plus;
            } else if (div_ten.rem < 3) {
                quality_i = q_minus;
            } else {
                quality_i = q_pure;
            }
        }

        /* Look up characters */
        letter = letters[letter_i];
        quality = qualities[quality_i];

        printf("%c%c\n", letter, quality);

    } 
    return(0); 
}






