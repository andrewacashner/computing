/* swapwords.c
 * Andrew Cashner, 2018/02/26
 *
 * Read a config file with key-value pairs;
 * read from standard input and write the text to standard output, replacing all
 * the keys with their value from the config file 
 */

#include<stdio.h>
#include<stdlib.h>

/* GLOBALS */
enum { 
    USAGE, 
    IO_CONFIGFILE,
    READ_YAML
} err_codes;

enum { 
    MAX_CHAR = 80,
    MAX_ENTRIES = 32
} max;
enum { 
    BODY, 
    KEY, 
    VALUE,
    DASH_OPEN, 
    DOT_CLOSE

} modes;

typedef struct dict_entry {
    char key[MAX_CHAR], value[MAX_CHAR];
} dict_entry;


/* FUNCTION PROTOTYPES */
int read_yaml(FILE *file, dict_entry kv_pairs[]);
void print_dict(dict_entry kv_pairs[], int max_pairs);
void error_msg(int errnum);

/* MAIN */
int main(int argc, char *argv[]) {

    char *configfile_name = NULL;
    FILE *configfile = NULL;
    int pairs;
    dict_entry kv_pairs[MAX_ENTRIES];
    
    if (argc != 2) {
        error_msg(USAGE);
        exit(EXIT_FAILURE);
    }
    
    configfile_name = argv[1];
    configfile = fopen(configfile_name, "r");
    if (configfile == NULL) {
        error_msg(IO_CONFIGFILE);
        exit(EXIT_FAILURE);
    }

    pairs = read_yaml(configfile, kv_pairs);
    if (pairs == 0) {
        error_msg(READ_YAML);
    }

    print_dict(kv_pairs, pairs);
    
    return(0);
}

/* FUNCTIONS */

int read_yaml(FILE *file, dict_entry kv_pairs[]) {
    int c, i, j;
    int mode = BODY;
    i = j = 0;
    
    while ((c = fgetc(file)) != EOF 
            && i < MAX_ENTRIES 
            && j < MAX_CHAR) {

        switch (mode) {
            case BODY:
                if (c == '.') {
                    mode = DOT_CLOSE;
                    continue;
                } else if (c == '-') {
                    mode = DASH_OPEN;
                    continue;
                } else {
                    mode = KEY;
                }
                /* fall through */
            case KEY:
                if (c == ':') {
                    kv_pairs[i].key[j] = '\0';
                    mode = VALUE;
                    j = 0;
                    continue;
                } else {
                    kv_pairs[i].key[j] = c;
                    ++j;
                    mode = KEY;
                } 
                break;
            case VALUE:
                if (c == '\n') {
                    kv_pairs[i].value[j] = '\0';
                    mode = BODY;
                    ++i;
                    j = 0;
                    continue;
                } else if (c == ' ' || c == '\t') {
                    continue;
                } else {
                    kv_pairs[i].value[j] = c;
                    ++j;
                    mode = VALUE;
                }
                break;
            case DASH_OPEN:
                if (c == '-') {
                    mode = DASH_OPEN;
                    continue;
                } else if (c == '\n') {
                    mode = BODY;
                    continue;
                }
                break;
            case DOT_CLOSE: 
                if (c == '.') {
                    mode = DOT_CLOSE;
                    continue;
                } else if (c == '\n') {
                    mode = BODY;
                    break;
                }
                break;
            default:
                mode = BODY;
                break;
        }

    }
    return(i);
}

void print_dict(dict_entry kv_pairs[], int max_pairs) {
    int i;
    for (i = 0; i < max_pairs; ++i) {
        printf("%s\t%s\n", kv_pairs[i].key, kv_pairs[i].value);
    }
    return;
}
            

void error_msg(int errnum) {
    switch (errnum) {
        case USAGE:
            printf("Usage: swapwords <config_file.yaml>\n");
            break;
        case IO_CONFIGFILE:
            printf("Could not open config file for reading.\n");
            break;
        case READ_YAML:
            printf("Error reading config file.\n");
        default:
            break;
    }
    return;
}

