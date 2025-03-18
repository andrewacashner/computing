/** Simple state machine with enum
 * 2025/03/04
 */

#include <stdio.h>

int main(void) {
    enum { OFF, ON } state = OFF;

    printf("Press 'x' to toggle the state ('q' to quit)\n\n");
    printf("OFF\n");
   
    int c;
    while ((c = getchar()) != 'q') {
        if (c == 'x') {
            switch (state) {
                case OFF:
                    printf("ON\n");
                    state = ON;
                    break;

                default:
                    printf("OFF\n");
                    state = OFF;
                    break;
            }
        }
    }

    return(0);
}
