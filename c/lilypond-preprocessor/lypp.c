/* lypp -- Andrew A. Cashner, 2017/02/22
 *
 * Lilypond preprocessor to generate lilypond score file from
 * input configuration file 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_STRING 160

#define MAX_MODES 4
enum {
    VERSION,
    INCLUDE,
    HEADER,
    SCORE,
    NORMAL
} mode_value;
char *mode_string[] = { "version",
    "include",
    "header",
    "score",
    "normal"
};

const char tabspaces[] = "  "; /* Tab = 2*/

/* Function prototypes */
char *trim_newline(char *string);

int main(int argc, char *argv[])
{
    FILE *infile;
    char line[MAX_STRING];
    char *line_ptr, *token, *test_str;
    int mode = NORMAL;
    bool in_section = false;
    int i;

    if (argc != 2) {
        fprintf(stderr, "Usage: lypp <inputfile>\n");
        exit(EXIT_FAILURE);
    }
    infile = fopen(argv[1], "r");
    if (infile == NULL) {
        fprintf(stderr, "Could not open file '%s' for reading.\n", argv[1]);
    }

    while (fgets(line, sizeof(line), infile) != NULL) {

        switch (mode) {
            case NORMAL:
                if (line[0] == '\n') {
                    continue;
                }
                if (line[0] == '#') {
                    line_ptr = trim_newline(&line[2]);
                    for (i = 0; i < MAX_MODES; ++i) {
                        if (strcmp(line_ptr, mode_string[i]) == 0) {
                            mode = i;
                        }
                    }
                    continue;
                } else {
                    printf("%s", line);
                }
                break;

           case VERSION:
                printf("\\version \"%s\"\n\n", trim_newline(line));
                mode = NORMAL; 
                break;

           case INCLUDE:
                if (line[0] == '\n') {
                    printf("\n");
                    mode = NORMAL;
                } else {
                    line_ptr = line;
                    for (test_str = line_ptr; ; test_str = NULL) {
                        token = strtok(test_str, ", \n");
                        if (token == NULL) {
                            break;
                        }
                        printf("\\include \"../ly/%s.ly\"\n", token);
                    }
                }
                break;

           case HEADER:
                if (in_section == false) {
                    printf("\\header {\n");
                    in_section = true;
                } 
                if (in_section == true) {
                    if (line[0] == '\n') {
                        printf("}\n\n");
                        in_section = false;
                        mode = NORMAL;
                    } else {
                        printf("%s%s", tabspaces, line);
                    }
                }
                break;

           case SCORE:
                if (in_section == false) {
                    printf("\\score {\n  <<\n");
                    in_section = true;
                } 
                if (in_section == true) {
                    if (line[0] == '\n') {
                        printf("%s>>\n}\n", tabspaces);
                        in_section = false;
                        mode = NORMAL;
                    } else {
                        line_ptr = line;
                        for (test_str = line_ptr; ; test_str = NULL) {
                            token = strtok(test_str, ":");
                            if (token == NULL) {
                                break;
                            }
                            printf("%s%s\\new %s\n%s%s<<\n", 
                                    tabspaces, tabspaces, token, tabspaces, tabspaces);
                        }
                        printf("\n%s%s>>\n", tabspaces, tabspaces);
                    }
                }
                break;

           default:
                mode = NORMAL;
                break;
        }
    }

    return(0);
}

char *trim_newline(char *string) {
    string[strlen(string) - 1] = '\0';
    return(string);
}


