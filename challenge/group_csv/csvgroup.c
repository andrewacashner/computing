/* Given a CSV with two columns (firstname, lastname), merge the two into one
 * and sort the results into two groups. Return a list of both groups,
 * separated by two newlines, to standard output.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR 128
#define MAX_LINES 512*2

char *read_filename(int, char**);
FILE *open_file(char*);
int populate_csv(char[][MAX_CHAR], FILE*);
void merge_cols(char*, char*);
int char_count(char*, char);

int main(int argc, char *argv[]) {
    char *filename;
    FILE *infile;
    char csv[MAX_LINES][MAX_CHAR] = {{0}};
    int i, rows;
    char line[MAX_CHAR];

    filename = read_filename(argc, argv);
    infile = open_file(filename);
    rows = populate_csv(csv, infile);

    for (i = 0; i < rows; ++i) {
        merge_cols(line, csv[i]);
        printf("%s\n", line);
    }

    /*
    for (i = 0; csv[i][0] != '\0'; ++i) {
        printf("%s\n", csv[i]);
    }
    */
   
    return(0);
}

char *read_filename(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: csvgroup FILENAME\n");
        exit(EXIT_FAILURE);
    }
    return(&argv[1][0]);
}

FILE *open_file(char *filename) {
    FILE *infile = fopen(filename, "r");
    if (infile == NULL) {
        fprintf(stderr, "Could not open file %s for reading.\n", filename);
        exit(EXIT_FAILURE);
    }
    return(infile);
}

int populate_csv(char csv[][MAX_CHAR], FILE *infile) {
        char line[MAX_CHAR];
        int i;
        int rows;

        for (i = 0; fgets(line, sizeof(line), infile) != NULL; ++i) {
            line[strlen(line) - 1] = '\0';
            strcpy(csv[i], line);
        }

        rows = i;

        for (; i < MAX_LINES; ++i) {
            csv[i][0] = '\0';
        }

        return(rows);
}

int char_count(char *s, char c) {
    int i, count;
    for (count = 0, i = 0; s[i] != '\0'; ++i) {
        if (s[i] == c) 
            ++count;
    }
    return(count);
}

int char_index(char *s, char c) {
    int i = 0;
    int index = -1;

    for (i = 0; s[i] != '\0'; ++i) {
        if (s[i] == 'c')
        {
            index = i;
            break;
        }
    }
    return(index);
}

/*
bool str_contains(char *s, char c) {
    return(char_count(s, c) > 0);
}
*/

void merge_cols(char *merged, char *csv_row) {
    char *first, *second;
    char row[MAX_CHAR];
    int count;
    strcpy(row, csv_row);
    count = char_count(row, ',');
    switch (count) {
        case 0:
            strcpy(merged, row);
            break;
        case 1:
            first = strtok(row, ",");
            second = strtok(NULL, ",");
            sprintf(merged, "%s %s", first, second);
            break;
        default:
            if (char_count(row, '"') > 0) {
                fprintf(stderr, "quote char found\n");
                /* Parse quoted phrases */
                if (char_index(row, '"') < char_index(row, ',')) {
                    printf("quote char found in first name");
                    /* Parse quoted firstname */
                    second = strrchr(row, ",");
                } else {
                    printf("quote char found in last name");
                    first = strtok(row, ",");
                    /* Parse quoted lastname */
                }
                sprintf(merged, "%s %s", first, second);
            } else {
                fprintf(stderr, "Could not parse\n");
            }
            merged[0] = '\0';
            break;
    }
}

