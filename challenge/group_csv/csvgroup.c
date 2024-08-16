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
void merge_cols(char*);
int char_count(char*, char);
char *quoted(char*);
void merge_quoted(char*, char*);
void print_csv(char[][MAX_CHAR]);

int main(int argc, char *argv[]) {
    char *filename;
    FILE *infile;
    char csv[MAX_LINES][MAX_CHAR] = {{0}};
    int i, rows;

    filename = read_filename(argc, argv);
    infile = open_file(filename);
    rows = populate_csv(csv, infile);

    for (i = 0; i < rows; ++i) {
        merge_cols(csv[i]);
    }

    print_csv(csv);
   
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
        if (s[i] == c) {
            index = i;
            break;
        }
    }
    return(index);
}

void merge_cols(char *csv_row) {
    char *first, *second;
    char row[MAX_CHAR];
    int count;
    strcpy(row, csv_row);
    /* TODO Need to check for quotes from beginning */
    count = char_count(row, ',');
    switch (count) {
        case 0:
            strcpy(csv_row, row);
            break;
        case 1:
            first = strtok(row, ",");
            second = strtok(NULL, ",");
            sprintf(csv_row, "%s %s", first, second);
            break;
        default:
            merge_quoted(csv_row, row);
            break;
    }
}

char *quoted(char *s) {
    if (char_index(s, '"') != 0) {
        fprintf(stderr, "Cannot parse string '%s'\n", s);
        exit(EXIT_FAILURE);
    }
    return(strtok(&s[1], "\""));
}

void merge_quoted(char *merged, char *s) {
    char *first, *second;

    if (char_count(s, '"') > 0) {
        if (char_index(s, '"') < char_index(s, ',')) {
            /* Quote in first name */
            second = strrchr(s, ',') + 1;
            first = quoted(s);
        } else {
            /* Quote in second name */
            second = strchr(strchr(s, ','), '\"');
            second = quoted(second);
            first = strtok(s, ",");
        } /* TODO what if both are quoted ? */
        sprintf(merged, "%s %s", first, second);
    } else {
        fprintf(stderr, "Cannot parse string '%s'\n", s);
        exit(EXIT_FAILURE);
    }
}

void print_csv(char csv[][MAX_CHAR]) {
    int i;
    for (i = 0; csv[i][0] != '\0'; ++i) {
        printf("%s\n", csv[i]);
    }
}
