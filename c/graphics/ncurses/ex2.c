/* ncurses example, invisible-island.net/ncurses/ncurses-intro.html
 * Forms
 * 2017-03-01
 */

#include <stdlib.h>
#include <curses.h>
#include <form.h>

#define MAX_FIELDS 5

int main(void) {

    FIELD *field[MAX_FIELDS + 1]; 
    FORM *my_form;
    int ch;
    int i;

    /* Initialize curses */
    initscr();
    start_color();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
   
    /* Initialize colors */
    init_pair(1, COLOR_BLACK, COLOR_WHITE);
    init_pair(2, COLOR_WHITE, COLOR_CYAN);

    /* Initialize the fields */
    for (i = 0; i < MAX_FIELDS; ++i) {
        field[i] = new_field(1, 10, 1, 18, 0, 0);
        move_field(field[i], i, 0);
    }
    field[i] = NULL;

    /* Set field options */
    /* Print a line for the option */
    for (i = 0; i < MAX_FIELDS; ++i) {
        set_field_fore(field[i], COLOR_PAIR(1));
        set_field_back(field[i], COLOR_PAIR(2));
    }

    /* Create the form and post it */
    my_form = new_form(field);
    post_form(my_form);
    refresh();

    set_current_field(my_form, field[0]);
    mvprintw(LINES - 2, 0, "Use UP, DOWN arrow keys to switch between fields");
    refresh();

    /* Loop through to get user requests */
    while ((ch = getch()) != 'q') {
        switch (ch) {
            case KEY_DOWN:
                /* Go to next field */
                form_driver(my_form, REQ_NEXT_FIELD);
                /* Go to the end of the present buffer */
                /* Leaves nicely at the last character */
                form_driver(my_form, REQ_END_LINE);
                break;
            case KEY_UP:
                /* Go to previous field */
                form_driver(my_form, REQ_PREV_FIELD);
                form_driver(my_form, REQ_END_LINE);
                break;
            case KEY_RIGHT:
                form_driver(my_form, REQ_NEXT_CHAR);
                break;
            case KEY_LEFT:
                form_driver(my_form, REQ_PREV_CHAR);
                break;
            case ' ':
                form_driver(my_form, ' ');
                break;
            default:
                break;
        }
    }

    /* Un-post form and free the memory */
    unpost_form(my_form);
    free_form(my_form);
    for (i = 0; i < MAX_FIELDS; ++i) {
        free_field(field[i]);
    }

    endwin();
    return(0);
}


