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
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

/* CONSTANTS, LOOKUP TABLES */
#define MAX_STR 80
#define MAX_LINE MAX_STR*4

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
char *default_outfile_name = "kwindex.md";
const char *delimiter = ";";
const char *search_string = "# Keywords";

/* GLOBAL DATA STRUCTURES */
/* Node for singly-linked list of index data */
typedef struct node *node_ptr;
struct node {
    char word[MAX_STR];         /* Keyword for index */
    char sort_word[MAX_STR];    /* Version of word for sort order */
    char filename[MAX_STR];     /* Source of keyword */
    node_ptr next;              /* Address of next node in list */
} node;

/* FUNCTION PROTOTYPES */
void quit_msg(int);
void quit_error_msg(int, char[]);
int find_keywords(FILE*, char*, FILE*);
node_ptr create_index(FILE*, FILE*);
node_ptr list_create_node(char*, char*);
node_ptr list_insert_sorted(node_ptr, char*, char*);
void list_print(FILE*, node_ptr);
void list_delete(node_ptr);
char *trim_initial_whitespace(char*);
char *convert_sort_format(char*);


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
    auxfile = tmpfile();
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
    list_print(outfile, index);

    /* Clean up */
    list_delete(index);
    fclose(outfile);
    fclose(auxfile);
    return (0);
}

/* FUNCTIONS */

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


/* FUNCTION find_keywords 
 * For the given input file, search for a section of keywords
 * and write them to auxiliary file
 * Before printing keywords, write name of source file from which they were
 * taken, followed by '::'.
 * End each group of keywords taken from a single file with '::' again.
 * We have ensured the files are open and valid, and we will close them
 * elsewhere.
 * RETURN: number of lines of keywords found
 *
 * TODO doesn't work, fix filename strtok algorithm
*/
int find_keywords(FILE *infile, char *infile_name, FILE *auxfile) {

    bool found = false;
    int c = 0, 
        linecount = 0;
    char line[MAX_LINE] = "";
    
    assert(infile != NULL && infile_name != NULL && auxfile != NULL);

    while ((c = fgetc(auxfile)) != EOF) {
        /* Just go to end of aux file */
        /* TODO do this with fseek? */
        ;
    }

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
                /* End line with delimiter so that multiple files don't run
                 * together */
                strcpy(&line[strlen(line) - 1], delimiter);
                fprintf(auxfile, "%s", line);
            }
        } else if (found == false) {
            /* Look for `# Keywords` heading */
            if (strncmp(line, search_string, strlen(search_string)) == 0) {
                found = true;
                /* Put source of index terms first in auxfile entry */
                fprintf(auxfile, "%s::", infile_name); 
            }
        }
    }
    if (found == true) {
        /* At end of section, add closing tag for source filename */
        fprintf(auxfile, "::");
    }
    return(linecount);
}

/* FUNCTION create_index 
 * Make a sorted index from keywords in the aux file
 * Read the keywords from the auxiliary file, separated by the delimiter;
 * enter them in sorted order into a linked list.
 * RETURN: Linked list in sorted order
 */
node_ptr create_index(FILE *outfile, FILE *auxfile) {
    char line[MAX_LINE] = "";
    char *keyword_ptr = NULL,
         *sourcefile_ptr = NULL;
    node_ptr index = NULL;

    assert(outfile != NULL && auxfile != NULL);
    rewind(auxfile);

    while (fgets(line, sizeof(line), auxfile) != NULL) {
        sourcefile_ptr = strtok(line, "::");
        keyword_ptr = strtok(line, delimiter);
        while (sourcefile_ptr != NULL) {
            while (keyword_ptr != NULL) {
                keyword_ptr = trim_initial_whitespace(keyword_ptr);
                index = list_insert_sorted(index, keyword_ptr, sourcefile_ptr);
                keyword_ptr = strtok(NULL, delimiter);
            }
            sourcefile_ptr = strtok(line, "::");
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
    
    strcpy(new_node->word, keyword);
    strcpy(new_node->sort_word, convert_sort_format(keyword));
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
    bool found = false;
    node_ptr list = NULL,
             new_node = list_create_node(keyword, sourcefile);

    /* Start new list from new node if head is empty */
    if (head == NULL) {
        return(new_node);
    } else if (strcmp(new_node->sort_word, head->sort_word) <= 0) {
        /* If new_node goes at beginning of list,
         * connect it to list and return new_node as the head */
        new_node->next = head;
        return(new_node);
    } else {
        /* Otherwise find insertion point and insert */
        for (list = head; list->next != NULL; list = list->next) {
            if (strcmp(new_node->sort_word, list->next->sort_word) <= 0) {
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

/* FUNCTION list_print
 * Print the data from the index linked-list in order, in specified format, to
 * given output file;
 * Recursive function
 * RETURN void
 */
void list_print(FILE *outfile, node_ptr list) {
    if (list != NULL) {
        fprintf(outfile, "%s | %s\n", list->word, list->filename);
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

/* FUNCTION trim_initial_whitespace 
 * Move start of string pointer to first
 * non-whitespace character
 * RETURN: address of start of new string
 */
char *trim_initial_whitespace(char *string) {
    int i = 0;

    assert(string != NULL);

    for (i = 0; string[i] != '\0'; ++i) {
        if (isspace(string[i]) == false) {
            break;
        }
    }
    /* Move pointer past initial whitespace */
    return(&string[i]);
}

/* FUNCTION convert_sort_format 
 * Take a string and convert it to a specified format for sorting:
 *   - Convert all uppercase letters to lowercase
 * RETURN: address of reformatted string 
 *
 * TODO: 
 *   - Convert accented characters to their non-accented equivalents
 *   - Or sort them specially without this function and without the sort_word
 *   element in the data structure
 * Replace with functions from locale.h and/or ICU collator for UTF-8 sorting?
 * (http://userguide.icu-project.org/collation/api)
 */

char *convert_sort_format(char *string) {
    int i = 0, c = 0;

    assert(string != NULL);

    for (i = 0; string[i] != '\0'; ++i) {
        c = string[i];
        if (isupper(c) != false) {
            string[i] = tolower(c);
        }
    }
    return(string);
}


