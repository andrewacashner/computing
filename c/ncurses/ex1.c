/* ncurses example, invisible-island.net/ncurses/ncurses-intro.html
 * 2017-03-01
 */

#include <stdlib.h>
#include <curses.h>
#include <signal.h>

static void finish(int sig);

int main(int argc, char *argv[]) {
    int num = 0;

    /* Initialize non-curses data structures here */

    (void) signal(SIGINT, finish);  /* Arrange interrupts to terminate */
    (void) initscr();               /* Initialize the curses library */
    keypad(stdscr, TRUE);           /* Enable keyboard mapping */
    (void) nonl();                  /* Tell curses not to do NL->CR/NL on output */
    (void) cbreak();                /* Take input chars one at a time, no wait for \n */
    (void) echo();                  /* Echo input -- in color */
    
    if (has_colors()) {
        start_color();

        /* Simple color assignment, often all we need. Color pair 0 cannot be
         * redefined. This example uses the same value for the color pair as for
         * the foreground color, though of course that is not necessary:
         */
        init_pair(1, COLOR_RED,     COLOR_BLACK);
        init_pair(2, COLOR_GREEN,   COLOR_BLACK);
        init_pair(3, COLOR_YELLOW,  COLOR_BLACK);
        init_pair(4, COLOR_BLUE,    COLOR_BLACK);
        init_pair(5, COLOR_CYAN,    COLOR_BLACK);
        init_pair(7, COLOR_MAGENTA, COLOR_BLACK);
        init_pair(8, COLOR_WHITE,   COLOR_BLACK);
    }

    for (;;)
    {
        int c = getch();    /* Refresh, accept single keystroke of input */
        attrset(COLOR_PAIR(num % 8));
        num++;

        /* Process the command keystroke */
    }

    finish(0); /* We're done */
}

static void finish(int sig) {
    endwin();

    /* Do your non-curses wrapup here */

    exit(0);
}
