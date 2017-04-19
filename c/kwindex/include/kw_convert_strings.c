/* kw_convert_strings.c -- Andrew A. Cashner, 2017/04/19
 * Functions for string conversion in kwindex
 */

#include "kw_convert_strings.h"

/* FUNCTION convert_trim_whitespace 
 * Find position of first non-whitespace character.
 * RETURN address (char ptr) of string starting at that position
 */
char *convert_trim_whitespace(char *string) {
    int i = 0;
    char *string_ptr = NULL;
    
    assert(string != NULL);

    for (i = 0; string[i] != '\0'; ++i) {
        if (isspace(string[i]) == false) {
           break;
        }
    }
    string_ptr = &string[i];
    return(string_ptr);
}

/* FUNCTION convert_sort_format 
 * Take a string and convert it to a specified format for sorting:
 *   - Convert all uppercase letters to lowercase
 * RETURN void
 *
 * TODO: 
 *   - Convert accented characters to their non-accented equivalents
 *   - Or sort them specially without this function and without the sort_word
 *   element in the data structure
 * Replace with functions from locale.h and/or ICU collator for UTF-8 sorting?
 * (http://userguide.icu-project.org/collation/api)
 */
void convert_sort_format(char *format_word, char *orig_word) {
    int i = 0, c = 0;

    assert(format_word != NULL && orig_word != NULL);

    strcpy(format_word, orig_word);

    for (i = 0; format_word[i] != '\0'; ++i) {
        c = format_word[i];
        if (isupper(c) != false) {
            format_word[i] = tolower(c);
        }
    }
    return;
}

/* FUNCTION convert_filename_format
 * Strip input suffix and path from given filename and format string as a
 * Markdown link 
 * RETURN void 
 */
void convert_filename_format(char *format_filename, char *orig_filename) {
    char tmp_str[MAX_STR] = "",
         basename[MAX_STR] = "";
    char *basename_ptr = NULL,
         *file_ext_ptr = NULL;

    assert(format_filename != NULL && orig_filename != NULL);

    strcpy(tmp_str, orig_filename);

    /* Strip off directory path: Move pointer to just past the last '/' in string;
     * if not found (strrchr returns NULL pointer), then use the whole string */
    basename_ptr = strrchr(tmp_str, '/');
    if (basename_ptr != NULL) {
        ++basename_ptr;
        strcpy(basename, basename_ptr);
    } else {
        strcpy(basename, tmp_str);
    }
   
    /* Strip suffix: Move pointer to start of file extension and if found,
     * replace the character at that position with the end-of-string '\0'
     * character */
    file_ext_ptr = strstr(basename, ".md");
    if (file_ext_ptr != NULL) {
        *file_ext_ptr = '\0'; 
    }

    /* Write the format_filename string in the proper format */
    sprintf(format_filename, "[%s](%s%s)", basename, basename, ".html");
    return;
}


