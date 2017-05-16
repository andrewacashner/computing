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
 * Encapsulate SQL functions
 * Specify output directory
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
#include <sqlite3.h>


/* DEFAULT VALUES */
char *default_outfile_name      = "kwindex.md";
char *default_db_name           = "kwindex.db";

/* Character delimiters used in find_keywords & create_keyword_list functions */
const char *keyword_delimiter   = ";",
      *filename_delimiter       = ":",
      *filegroup_delimiter      = "|",
      *header_string            = "# Keywords";

/* For constructing COLLATE statements that ignore case and accents */
const char ascii_chars[] = "aeioun";
const int unicode_char_sub_max[] = {
    10, 10, 10, 10, 10, 2
};
const char *unicode_char_sub_a[] = {
        "á", "à", "â", "ä", "ã", "Á", "À", "Â", "Ä", "Ã",
    },
    *unicode_char_sub_e[] = {
        "é", "è", "ê", "ë", "ẽ", "É", "È", "Ê", "Ë", "Ẽ",
    },
    *unicode_char_sub_i[] = {
        "í", "ì", "î", "ï", "ĩ", "Í", "Ì", "Î", "Ï", "Ĩ",
    },
    *unicode_char_sub_o[] = {
        "ó", "ò", "ô", "ö", "õ", "Ó", "Ò", "Ô", "Ö", "Õ",
    },
    *unicode_char_sub_u[] = {
        "ú", "ù", "û", "ü", "ũ", "Ú", "Ù", "Û", "Ü", "Ũ",
    },
    *unicode_char_sub_n[] = {
        "ñ", "Ñ"
    };
const char **unicode_char_sub[] = {
    unicode_char_sub_a,
    unicode_char_sub_e,
    unicode_char_sub_i,
    unicode_char_sub_o,
    unicode_char_sub_u,
    unicode_char_sub_n
};


const int unicode_char_sub_index[] = {
    0, 11, 22, 33, 44, 55, 58
};
#define MAX_UNICODE_CHAR_INDEX

const char *sql_string[] = {
        "CREATE TABLE IF NOT EXISTS keywords (word, file, PRIMARY KEY (word, file))", 

        "INSERT OR IGNORE INTO keywords (word, file) VALUES (?1, ?2)",

        "SELECT word, GROUP_CONCAT(file, ', ') " 
            "FROM (SELECT word, file FROM keywords ORDER BY file) GROUP BY word "
    };
enum { 
    KW_SQL_CREATE_TABLE,
    KW_SQL_INSERT,
    KW_SQL_SELECT_KEYWORDS,
    KW_SQL_MAX
} kw_sql_string_code;
enum {
    KW_SQL_TEST_GOOD,
    KW_SQL_TEST_BAD
} kw_sql_error_code;

#define SQL_TEST(ERRCODE)  { sql_test = kw_sql_test(sql_test, ERRCODE, db); }

/* FUNCTION PROTOTYPES */
char *kw_sql_create_collate_stmt(char*);
int find_keywords(FILE*, char*, FILE*);
node_ptr create_keyword_list(FILE*);
int index_insert(sqlite3*, node_ptr, sqlite3_stmt*);
int index_file_print(FILE*, sqlite3*, sqlite3_stmt*);
int kw_sql_test(int, int, sqlite3*);

/* MAIN */
int main(int argc, char *argv[]) {
    FILE *outfile = NULL, 
         *auxfile = NULL,
         *infile = NULL;
    int  c = 0,
         keyword_lines = 0,
         sql_test = 0;
    char *outfile_name = default_outfile_name,
         *infile_name = NULL;
    node_ptr index = NULL;
    sqlite3 *db = NULL;
    sqlite3_stmt *sql_statement = NULL;

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
    outfile = fopen(outfile_name, "w+");
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
        /* Search input file for keywords and write them to auxfile */
        keyword_lines += find_keywords(infile, infile_name, auxfile);
        fclose(infile);
    } 

    if (keyword_lines == 0) {
        error_msg(NO_KEYWORDS_FOUND, NULL);
        goto cleanup;
    }

    /* Create linked list, unsorted, of keywords and filenames in auxfile */
    index = create_keyword_list(auxfile);
  
    /* Create sqlite database */
    sql_test = sqlite3_open(default_db_name, &db);
    if (sql_test != 0) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        exit(EXIT_FAILURE);
    }

    /* Create keywords table in db */
    sql_test = sqlite3_prepare_v2(db, 
            sql_string[KW_SQL_CREATE_TABLE], -1, &sql_statement, NULL);
    SQL_TEST(SQLITE_OK);
    sql_test = sqlite3_step(sql_statement);
    SQL_TEST(SQLITE_DONE);
    sql_test = sqlite3_reset(sql_statement);
    SQL_TEST(SQLITE_OK);

    /* Insert values from linked list into database */
    sql_test = index_insert(db, index, sql_statement);
    if (sql_test == 0) {
        fprintf(stderr, "No new keywords inserted into database.\n");
    }

    /* Print output in sorted order using SQLITE query */
    sql_test = index_file_print(outfile, db, sql_statement);
    if (sql_test == 0) {
        fprintf(stderr, "No new keywords found in database to print.\n");
    }
    
    /* Finish with SQLITE */ 
    sql_test = sqlite3_finalize(sql_statement);
    SQL_TEST(SQLITE_OK);

    /* Clean up */
cleanup:
    list_delete(index); 
    sqlite3_close(db);
    fclose(outfile);
    fclose(auxfile);
    return(0);
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

/* FUNCTION create_keyword_list 
 * Make a sorted index from keywords in the aux file
 * Read the filenames and keywords from the auxiliary file
 * enter them in sorted order into a linked list.
 * RETURN: Linked list in sorted order
 */
node_ptr create_keyword_list(FILE *auxfile) {
    char line[MAX_LINE] = "",
         filename[MAX_STR] = "",
         format_filename[MAX_STR] = "", 
         keyword_group[MAX_STR] = "",
         keyword[MAX_STR] = "";
    char *search_ptr = NULL;
    node_ptr new_node = NULL, 
             index = NULL;

    assert(auxfile != NULL);
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

/* FUNCTION index_insert 
 * Insert values from linked list into sqlite database
 * RETURN number of keywords inserted
 */
int index_insert(sqlite3 *db, node_ptr head, sqlite3_stmt *sql_statement) {
    int sql_test = 0,
        keywords = 0;
    node_ptr list = NULL;

    for (list = head; list != NULL; list = list->next) {
        sql_test = sqlite3_prepare_v2(db, 
                sql_string[KW_SQL_INSERT], -1, &sql_statement, NULL);
        SQL_TEST(SQLITE_OK);
        sql_test = sqlite3_bind_text(sql_statement, 1, list->word, -1, SQLITE_STATIC);
        SQL_TEST(SQLITE_OK);
        sql_test = sqlite3_bind_text(sql_statement, 2, list->filename, -1, SQLITE_STATIC);
        SQL_TEST(SQLITE_OK);
        sql_test = sqlite3_step(sql_statement);
        SQL_TEST(SQLITE_DONE);
        sql_test = sqlite3_reset(sql_statement);
        SQL_TEST(SQLITE_OK);
        ++keywords;
    }

    return(keywords);
}

/* FUNCTION kw_sql_create_collate_stmt
 * TODO This does not work, there are proper ways to create sqlite collation
 * sequences!
 *
 * Create a string to specify sqlite collation
 * "|| 'á' like 'a' || 'à' like 'a'" etc.
 *
 * Takes string pointer, allocates new string, and fills it with this statement;
 * data for statement are taken from table 'unicode_char_sub', where the 0 index
 * of each string is the character to use for sorting
 *
 * RETURN pointer to string with collation statement
 */ 
char *kw_sql_create_collate_stmt(char *sort_string) {
    int i = 0, 
        j = 0;
    char next_string[MAX_LINE] = "";

    assert(sort_string != NULL);

    for (i = 0; ascii_chars[i] != '\0'; ++i) {
        for (j = 0; j < unicode_char_sub_max[i]; ++j) {

            sprintf(next_string, "|| '%s' LIKE '%c' ", 
                    unicode_char_sub[i][j],
                    ascii_chars[i]);
            strcat(sort_string, next_string);
        }
    }
    DEBUG_PRINT(("kw_sql_create_collate_stmt: %s\n", sort_string));
    return(sort_string);
}



/* FUNCTION index_file_print
 * Print the index file with table format using SQLITE query on db
 * RETURN number of keywords printed
 */
int index_file_print(FILE *outfile, sqlite3 *db, sqlite3_stmt *sql_statement) {
    int sql_test = 0, 
        keywords = 0;
    char select_stmt[MAX_LINE] = "",
         sort_stmt[MAX_LINE] = "";

    /* Create full SELECT statement including COLLATE statement */
    strcpy(select_stmt, sql_string[KW_SQL_SELECT_KEYWORDS]);
/*    strcpy(sort_stmt, kw_sql_create_collate_stmt(sort_stmt));
    strcat(select_stmt, sort_stmt);
    DEBUG_PRINT(("index_file_print stmt: %s\n", select_stmt));
*/
    fprintf(outfile, "# Index of Keywords\n\n");

    sql_test = sqlite3_prepare_v2(db, select_stmt, -1, &sql_statement, NULL); 
    SQL_TEST(SQLITE_OK);

    for(keywords = 0; 
            (sql_test = sqlite3_step(sql_statement)) == SQLITE_ROW; 
            ++keywords) {
        fprintf(outfile, "| %s %s\n", 
                sqlite3_column_text(sql_statement, 0),
                sqlite3_column_text(sql_statement, 1));
    }
    sql_test = sqlite3_reset(sql_statement);
    SQL_TEST(SQLITE_OK);
   
    return(keywords);
}

/* FUNCTION kw_sql_test
 * Test the return value from a sqlite3 command against a desired return value
 * RETURN good or bad error code depending on result.
 */
int kw_sql_test(int test_value, int sql_desired_return, sqlite3 *db) {
    int result = 0;
    if (test_value != sql_desired_return) {
        fprintf(stderr, "%s\n", sqlite3_errmsg(db));
        result = KW_SQL_TEST_BAD;
    } else {
        result = KW_SQL_TEST_GOOD;
    }
    return(result);
}



