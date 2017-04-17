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
 *   - Search file for keywords section delimiters, save start and end locations,
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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
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
char *default_auxfile_name = "keywords.aux";

const char *search_string = "# Keywords\n";
#define SEARCH_STRLEN 12

/* FUNCTION PROTOTYPES */
void quit_msg(int);
void quit_error_msg(int, char[]);
int find_keywords(FILE*, FILE*);
int create_index(FILE*, FILE*);

/* MAIN */
int main(int argc, char *argv[]) {

    int c = 0;
    FILE *outfile = NULL, 
         *auxfile = NULL,
         *infile = NULL;
    char *outfile_name = default_outfile_name, 
         *auxfile_name = default_auxfile_name,
         *infile_name = NULL;
    int keyword_lines = 0;

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
    auxfile = fopen(auxfile_name, "w");
    if (auxfile == NULL) {
        quit_error_msg(WRITE_FILE_OPEN_FAILURE, auxfile_name);
    }

    /* Then open, check, and process input files */
    for ( ; optind < argc; ++optind) {
        infile_name = argv[optind];
        /* TODO move file opening & closing for infile & auxfile to function */
        infile = fopen(infile_name, "r");
        if (infile == NULL) {
            quit_error_msg(READ_FILE_OPEN_FAILURE, infile_name);
        } 
        keyword_lines += find_keywords(infile, auxfile);
        fclose(infile);
    } 

    if (keyword_lines == 0) {
        quit_error_msg(NO_KEYWORDS_FOUND, NULL);
    }
    create_index(outfile, auxfile);

    fclose(outfile);
    fclose(auxfile);

    return (0);
}

/* FUNCTIONS */

/* quit_msg -- Print a message to stdout and quit with no error code */
void quit_msg(int message_code) {
    assert(message_code >= 0 && message_code < MAX_MSG);
    printf("%s\n", message[message_code]);
    exit(0);
}

/* quit_error_msg -- Print a message to stderr and quit with error code */
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


/* find_keywords -- For the given input file, search for a section of keywords
 * and write them to auxiliary file
 * We have ensured the files are open and valid, and we will close them
 * elsewhere.
 * RETURN: number of lines of keywords found
*/
int find_keywords(FILE *infile, FILE *auxfile) {

    bool found = false;
    int linecount = 0;
    char line[MAX_LINE] = "";
    int c;
    
    assert(infile != NULL && auxfile != NULL);

    while ((c = fgetc(auxfile)) != EOF) {
        /* Just go to end of aux file */
        ;
    }

    while (fgets(line, sizeof(line), infile) != NULL) {
        if (found == true) {
            /* We have found a `# Keywords` heading;
             * now we search the keyword region:
             * we skip blank lines, count valid lines,
             * and look for end of section (= next section heading or EOF)
             */
            if (line[0] == ' ' || line[0] == '\n') {
                continue;
            } else if (line[0] == '#') {
                break;
            } else {
                ++linecount;
                fputs(line, auxfile);
            }
        } else if (found == false) {
            /* Look for `# Keywords` heading */
            if (strcmp(line, search_string) == 0) {
                found = true;
            }
        }
    }
    return(linecount);
}

int create_index(FILE *outfile, FILE *auxfile) {
/* TODO need to open auxfile for READING this time */
    int items = 0;
    char line[MAX_LINE] = "";
    char keyword[MAX_STR] = "";
    char *keyword_ptr = NULL;
    char delimiters[] = "; \n";

    assert(outfile != NULL && auxfile != NULL);

    while (fgets(line, sizeof(line), auxfile) != NULL) {
        if (items == 0) {
            keyword_ptr = strtok(line, delimiters);
        } else {
            keyword_ptr = strtok(NULL, delimiters);
        }
        sscanf(keyword_ptr, "%s", keyword);
        ++items;
        printf("%d %s\n", items, keyword);
    }

    return(items);
}

