/* kwindex.c -- Andrew A. Cashner, 2017/04/17
 * Indexing program for low-tech note-taking system in Markdown
 *
 * PURPOSE
 * Read keywords from markdown file and write an alphabetized index of the
 * keywords with links or references to the files in which they appear
 *
 * USAGE
 *     kwindex [-o outfile.md] infile1.md infile2.md infile3.md
 *     If output filename is not specified with -o [FILENAME], then default name
 *     will be used.
 *
 * INPUT
 * Markdown notes files will all have a section like this:
 *      ~~~
 *      # Keywords
 *         one; two; three; four, five; six
 *
 *      # Next section
 *      ~~~
 * The keyword section is delimited at the start by the line `# Keywords\n`;
 * and at the end by either another section header `# Text...` or the end of
 * file.
 * The keywords are separated by semicolons.
 *
 * OUTPUT
 * Output will be like this (assuming Markdown format for now, and just
 * filenames instead of links)
 *      ~~~
 *      # Index of Keywords
 *
 *      four,five   | input.md
 *      one         | input.md
 *      six         | input.md
 *      three       | input.md
 *      two         | input.md
 *      ~~~
 *
 * IMPLEMENTATION
 * Read and process command-line arguments and options 
 * Check input files, exit with error message if files not found or not valid
 * Create empty linked list for index words (structs with string + filename)
 * For each input file...
 *   - Search file for keywords section delimiter, save start and end locations,
 *     count bytes between
 *   - If found, allocate necessary memory and read keyword section into memory
 *      * If not, move on to next file; or if last file, exit
 *   - For all keywords found, parse keywords by delimeter, insert them in
 *     sorted order into linked list 
 *   - After last word, move on to next file and repeat search and sorted-insert
 * After last file...
 *   - Print linked list content in desired format to output file
 * Clean up and exit
 *
 */

/* TODO
 * Keep track of more data: e.g. filename from which keywords came for indexing
 * Do locale-based sorting (handle UTF-8 accents)
 * Too many void functions?
 * Separate modules with headers
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

/* DEBUGGING */
#ifdef DEBUG
#define DEBUG_PRINT(x) printf x
#else
#define DEBUG_PRINT(x) {};
#endif
/* CONSTANTS, LOOKUP TABLES */
#define MAX_STR 12*12
#define MAX_LINE MAX_STR*12

enum { 
    HELP,
    VERSION,
    NO_INFILE_SPECIFIED,
    READ_FILE_OPEN_FAILURE,
    WRITE_FILE_OPEN_FAILURE,
    NO_KEYWORDS_FOUND,
    MAX_MSG
} message_code;

const char *message[] = {
    /* TODO: Supply real messages */
    "Help message.",
    "Version message.",
    "No input file specified.",
    "Could not open file for reading.",
    "Could not open file for writing.",
    "No keywords found."
};

/* DEFAULT VALUES */
char *default_outfile_name      = "kwindex.md";
const char *keyword_delimiter   = ";",
      *filename_delimiter       = ":",
      *filegroup_delimiter      = "|",
      *header_string            = "# Keywords";

/* GLOBAL DATA STRUCTURES */
/* Node for singly-linked list of index data */
typedef struct node *node_ptr;
struct node {
    char word[MAX_STR],         /* Keyword for index */
         sort_word[MAX_STR],    /* Version of word for sort order */
         filename[MAX_LINE];    /* Source of keyword */
    node_ptr next;              /* Address of next node in list */
} node;

/* FUNCTION PROTOTYPES */
void quit_msg(int);
void quit_error_msg(int, char[]);
int find_keywords(FILE*, char*, FILE*);
node_ptr create_index(FILE*, FILE*);
node_ptr list_create_node(char*, char*);
node_ptr list_insert_sorted(node_ptr, char*, char*);
void list_insert_duplicate(node_ptr, node_ptr);
void index_file_print(FILE*, node_ptr);
void list_print(FILE*, node_ptr);
void list_delete(node_ptr);
char *convert_trim_whitespace(char*);
void convert_sort_format(char*, char*);
void convert_filename_format(char*, char*);


/* MAIN */
int main(int argc, char *argv[]) {

    int c = 0,
        keyword_lines = 0;
    FILE *outfile = NULL, 
         *auxfile = NULL,
         *infile = NULL;
    char *outfile_name = default_outfile_name, 
         *infile_name = NULL;
    node_ptr index = NULL;

    /* Process command-line arguments */
    while ((c = getopt(argc, argv, "hvo:")) != -1) {
        switch (c) {
            case 'h':
                quit_msg(HELP);
            case 'v':
                quit_msg(VERSION);
            case 'o':
                /* Take output filename from argument */
                outfile_name = optarg;
                break;
            default:
                exit(EXIT_FAILURE);
        }
    }
    if (optind == argc) {
        /* If no more arguments */
        quit_error_msg(NO_INFILE_SPECIFIED, NULL);
    } 

    /* Open and check specified or default output file */
    outfile = fopen(outfile_name, "w");
    if (outfile == NULL) {
        quit_error_msg(WRITE_FILE_OPEN_FAILURE, outfile_name);
    }
    /* Open and check auxiliary file */
#ifdef DEBUG
    auxfile = fopen("kwindex.aux", "w+");
#else
    auxfile = tmpfile();
#endif
    if (auxfile == NULL) {
        quit_error_msg(WRITE_FILE_OPEN_FAILURE, "temporary file");
    }

    /* Then open, check, and process input files */
    for ( ; optind < argc; ++optind) {
        infile_name = argv[optind];
        infile = fopen(infile_name, "r");
        if (infile == NULL) {
            quit_error_msg(READ_FILE_OPEN_FAILURE, infile_name);
        } 
        keyword_lines += find_keywords(infile, infile_name, auxfile);
        fclose(infile);
    } 

    if (keyword_lines == 0) {
        quit_error_msg(NO_KEYWORDS_FOUND, NULL);
    }
    index = create_index(outfile, auxfile);
    index_file_print(outfile, index);

    /* Clean up */
    list_delete(index); 
    fclose(outfile);
    fclose(auxfile);
    return (0);
}

/************************************
 * FUNCTIONS 
 ************************************/

/* EXIT AND ERROR FUNCTIONS */

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


/* PARSING AND INDEXING FUNCTIONS */

/* FUNCTION find_keywords 
 * For the given input file, search for a section of keywords
 * and write them to auxiliary file as follows:
 *   filename1:
 *   keyword1; keyword2; keyword3;
 *
 *   filename2:
 *   keyword4; keyword5
 *
 * We have ensured the files are open and valid, and we will close them
 * elsewhere.
 * RETURN: number of lines of keywords found
*/
int find_keywords(FILE *infile, char *infile_name, FILE *auxfile) {

    bool found = false;
    int linecount = 0;
    char line[MAX_LINE] = "";
    
    assert(infile != NULL && infile_name != NULL && auxfile != NULL);

    /* Go to end of aux file */
    fseek(auxfile, 0L, SEEK_END);

    while (fgets(line, sizeof(line), infile) != NULL) {
        if (found == true) {
            /* We have found a `# Keywords` heading;
             * now we search the keyword region:
             * we skip blank lines, count valid lines,
             * and look for end of section (= next section heading or EOF)
             */
            if (isspace(line[0]) != false) {
                continue;
            } else if (line[0] == '#') {
                break;
            } else {
                ++linecount;
                /* End line with keyword_delimiter so that multiple lines or files don't run
                 * together as a single keyword */
                strcpy(&line[strlen(line) - 1], keyword_delimiter);
                fprintf(auxfile, "%s", line);
            }
        } else if (found == false) {
            /* Look for `# Keywords` heading */
            if (strncmp(line, header_string, strlen(header_string)) == 0) {
                found = true;
                /* Put source of index terms first in auxfile entry */
                fprintf(auxfile, "%s%s\n", infile_name, filename_delimiter); 
            }
        }
    }
    if (found == true) {
        /* At end of section, add closing tag for source filename */
        fprintf(auxfile, "%s\n", filegroup_delimiter);
    }
    return(linecount);
}

/* FUNCTION create_index 
 * Make a sorted index from keywords in the aux file
 * Read the filenames and keywords from the auxiliary file
 * enter them in sorted order into a linked list.
 * RETURN: Linked list in sorted order
 * TODO doesn't work, fix strtok for keywords?
 */
node_ptr create_index(FILE *outfile, FILE *auxfile) {
    char line[MAX_LINE] = "",
         filename[MAX_STR] = "",
         format_filename[MAX_STR] = "",
         keyword_group[MAX_STR] = "",
         keyword[MAX_STR] = "";
    char *search_ptr = NULL;
    node_ptr index = NULL;

    assert(outfile != NULL && auxfile != NULL);
    rewind(auxfile);

    /* Find keywords separated by keyword_delimiter and enter them into index
     * linked list in sorted order
     */
    while (fgets(line, sizeof(line), auxfile) != NULL) {
        /* Find filename in format "%s:" */
        strcpy(filename, strtok(line, filename_delimiter));
        convert_filename_format(format_filename, filename);
        
        /* Read another line to find keywords */
        if (fgets(line, sizeof(line), auxfile) != NULL) {

            /* Find group of keywords after filename in format "%s|" */
            strcpy(keyword_group, strtok(line, filegroup_delimiter)); 

            /* Find individual keywords within that group in format "%s;" */
            search_ptr = strtok(keyword_group, keyword_delimiter);
            while (search_ptr != NULL) {
                
                /* Insert this keyword, minus initial whitespace, and this
                 * filename into index */
                search_ptr = convert_trim_whitespace(search_ptr);
                strcpy(keyword, search_ptr);
                index = list_insert_sorted(index, keyword, format_filename);
                
                /* Find next keyword */
                search_ptr = strtok(NULL, keyword_delimiter);
            } 
        } 
    }
    return(index);
}


/* FUNCTION list_create_node
 * Create a new node (allocating necessary memory) for linked list using given
 * data
 * RETURN address of node in node_ptr
 */
node_ptr list_create_node(char *keyword, char *sourcefile) {
    node_ptr new_node = (node_ptr)malloc(sizeof(node));
    char format_sort_word[MAX_STR] = "";
    
    strcpy(new_node->word, keyword);

    convert_sort_format(format_sort_word, keyword);
    strcpy(new_node->sort_word, format_sort_word);
    
    strcpy(new_node->filename, sourcefile);
    
    new_node->next = NULL;
  
    return(new_node);
}

/* FUNCTION list_insert_sorted
 * Insert a new node in correct sort order into a linked list
 * Create a new linked list if necessary
 * RETURN node_ptr containing address of head of sorted list
 */
node_ptr list_insert_sorted(node_ptr head, char *keyword, char *sourcefile) {
    int strtest = 0;
    bool found = false;
    node_ptr list = NULL,
             new_node = list_create_node(keyword, sourcefile);

    /* Start new list from new node if head is empty */
    if (head == NULL) {
        return(new_node);
    } 
    strtest = strcmp(new_node->sort_word, head->sort_word);
    if (strtest == 0) {
        /* If new keyword is already in index, add filename to existing index
         * node and free unused new_node */
        list_insert_duplicate(head, new_node);
    } else if (strtest < 0) {
        /* If new_node goes at beginning of list,
         * connect it to list and return new_node as the head */
        new_node->next = head;
        return(new_node);
    } else {
        /* Otherwise find insertion point and insert */
        /* TODO find a way to avoid duplicating code here */
        for (list = head; list->next != NULL; list = list->next) {
            
            strtest = strcmp(new_node->sort_word, list->next->sort_word);
            if (strtest == 0) {
                /* Duplicate found */ 
                list_insert_duplicate(list, new_node);
                break;
            
            } else if (strtest < 0) {
                /* Insertion point found, insert new_node here */
                new_node->next = list->next;
                list->next = new_node;
                found = true;
                break;
            }
        }
        if (found == false) {
            /* No insertion point found, so append to list */
            list->next = new_node;
        }
    }
    return(head);
}

/* FUNCTION list_insert_duplicate
 * When a duplicate keyword is found, we do not need to add a new node to the
 * list; we just need to add a new filename to the filename field of the current
 * node, then we can free the unused new node that was created
 * RETURN void
 */
void list_insert_duplicate(node_ptr match_node, node_ptr new_node) {
    assert(match_node != NULL && new_node != NULL);
  
    strcat(match_node->filename, ", ");
    strcat(match_node->filename, new_node->filename);
    free(new_node); 
    return;
}


/* FUNCTION index_file_print
 * Print the index file with table format
 * Print header and footer, and call list_print to print the internal contents
 * recursively
 * RETURN void
 */
void index_file_print(FILE *outfile, node_ptr list) {
    if (list != NULL) {
        fprintf(outfile, "# Index of Keywords\n\n"
                "--------------------  --------------------\n");
        list_print(outfile, list);
        fprintf(outfile, 
                "--------------------  --------------------\n");
    }
    return;
}

/* FUNCTION list_print
 * Print the data from the index linked-list in order, in specified format, to
 * given output file;
 * Recursive function
 * RETURN void
 */
void list_print(FILE *outfile, node_ptr list) {
    if (list != NULL) {
        fprintf(outfile, "%-20s  %s\n", list->word, list->filename);
        list_print(outfile, list->next);
    }
    return;
}

/* FUNCTION list_delete
 * Free the memory used for the linked-list index;
 * Using recursive call to access all list elements
 * RETURN void
 */
void list_delete(node_ptr list) {
    if (list != NULL) {
        list_delete(list->next);
    }
    free(list);
    return;
}

/* STRING PROCESSING FUNCTIONS */

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


