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
 * Do locale-based sorting (can't currently handle UTF-8 accents properly)
 * Too many void functions?
 * Separate more modules with headers
 */

#include "kw_debug_print.h"
#include "kw_max_sizes.h"
#include "kw_message.h"
#include "kw_convert_strings.h"
#include "kw_list.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <locale.h>


/* DEFAULT VALUES */
char *default_outfile_name      = "kwindex.md";
/* Character delimieters used in find_keywords & create_index functions */
const char *keyword_delimiter   = ";",
      *filename_delimiter       = ":",
      *filegroup_delimiter      = "|",
      *header_string            = "# Keywords";

/* FUNCTION PROTOTYPES */
int find_keywords(FILE*, char*, FILE*);
node_ptr create_index(FILE*, FILE*);
void index_file_print(FILE*, node_ptr);

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

    setlocale(LC_ALL, "");

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

/* FUNCTION find_keywords 
 * For the given input file, search for a section of keywords
 * and write them to auxiliary file as follows:
 *   filename1:
 *   keyword1; keyword2; keyword3;|
 *
 *   filename2:
 *   keyword4; keyword5|
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
 */
node_ptr create_index(FILE *outfile, FILE *auxfile) {
    char line[MAX_LINE] = "",
         filename[MAX_STR] = "",
         format_filename[MAX_STR] = "", 
         keyword_group[MAX_STR] = "",
         keyword[MAX_STR] = "";
    char *search_ptr = NULL;
    node_ptr new_node = NULL, 
             index = NULL;

    assert(outfile != NULL && auxfile != NULL);
    rewind(auxfile);

    /* Find filenames and keywords and create an unsorted linked list of nodes
     * containing these data. */

    while (fgets(line, sizeof(line), auxfile) != NULL) {
        /* Find filename in format "%s:" */
        strcpy(filename, strtok(line, filename_delimiter));
        convert_filename_format(format_filename, filename);
        
        /* Read another line to find keywords */
        if (fgets(line, sizeof(line), auxfile) != NULL) {

            /* Find group of keywords after filename in format "%s|" */
            strcpy(keyword_group, strtok(line, filegroup_delimiter));

            /* Find individiual keywords within that group in format "%s;" */
            search_ptr = strtok(keyword_group, keyword_delimiter);
            while (search_ptr != NULL) {

                /* Create a new node with this keyword, minus initial
                 * whitespace, and this formatted filename; insert unsorted into
                 * index */
                search_ptr = convert_trim_whitespace(search_ptr);
                strcpy(keyword, search_ptr);
                new_node = list_create_node(keyword, format_filename);
                index = list_append(index, new_node);

                /* Find next keyword */
                search_ptr = strtok(NULL, keyword_delimiter);
            }
        }
    }
    return(index);
}


/* FUNCTION index_file_print
 * Print the index file with table format
 * Print header and footer, and call list_print to print the internal contents
 * recursively
 * RETURN void
 */
void index_file_print(FILE *outfile, node_ptr list) {
    if (list != NULL) {
        fprintf(outfile, "# Index of Keywords\n\n");
        list_print(outfile, list);
    }
    return;
}
