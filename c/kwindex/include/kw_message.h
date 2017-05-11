/* kw_message.h -- Andrew Cashner, 2017/04/19
 * Header file for command-line messages 
 */

#ifndef KW_MESSAGE_H
#define KW_MESSAGE_H

#include "kw_debug_print.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

enum { 
    HELP,
    VERSION,
    NO_INFILE_SPECIFIED,
    READ_FILE_OPEN_FAILURE,
    WRITE_FILE_OPEN_FAILURE,
    NO_KEYWORDS_FOUND,
    MAX_MSG
} message_codes;

/* Exit program successfully after displaying message (e.g., help) */
extern void quit_msg(int);

/* Exit program with error code and message (e.g., file read error) */
extern void quit_error_msg(int, char[]);

/* Just print error message but don't quit */
extern void error_msg(int, char[]);

#endif /* KW_MESSAGE_H */
