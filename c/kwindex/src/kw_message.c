/* kw_message.c -- Andrew Cashner, 2017/04/19
 * Functions for command-line messages in kwindex
 */

#include "kw_message.h"

/* FUNCTION quit_msg 
 * Print a message to stdout and quit with no error code 
 * RETURN void
 */
void quit_msg(int message_code) {
    assert(message_code >= 0 && message_code < MAX_MSG);
    printf("%s\n", message[message_code]);
    exit(0);
}

/* FUNCTION quit_error_msg 
 * Print a message to stderr and quit with error code 
 * RETURN void
 */
void quit_error_msg(int message_code, char detail_msg[]) {
    assert(message_code >= 0 && message_code < MAX_MSG);
    if (detail_msg != NULL) {
        /* TODO: redo with varargs? 
         * e.g., to allow error message like "Could not open %s for reading"
         */
        fprintf(stderr, "%s (%s)\n", message[message_code], detail_msg);
    } else {
        fprintf(stderr, "%s\n", message[message_code]);
    }
    exit(EXIT_FAILURE);
}


