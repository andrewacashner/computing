/* kw_convert_strings.h -- Andrew A. Cashner, 2017/04/19
 * Header file for string conversion functions in kwindex
 */

#ifndef KW_CONVERT_STRINGS_H
#define KW_CONVERT_STRINGS_H

#include "kw_debug_print.h"
#include "kw_max_sizes.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <assert.h>
#include <locale.h>


/* Trim initial whitespace from string */
extern char *convert_trim_whitespace(char*);

/* Convert keyword into desired format for sorting */
extern void convert_sort_format(char*, char*);

/* Convert filename into desired format for index file */
extern void convert_filename_format(char*, char*);

#endif /* KW_CONVERT_STRINGS_H */


