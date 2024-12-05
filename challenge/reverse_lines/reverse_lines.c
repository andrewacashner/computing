#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void record_file_dimensions(FILE*, int[2]);

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: reverseLines INFILE OUTFILE\n");
        exit(EXIT_FAILURE);
    }

    char *infile_name = argv[1];
    char *outfile_name = argv[2];

    FILE *infile = fopen(infile_name, "r");
    if (infile == NULL) {
        fprintf(stderr, "Could not open file %s for reading\n", infile_name);
        exit(EXIT_FAILURE);
    }

    FILE *outfile = fopen(outfile_name, "w");
    if (outfile == NULL) {
        fprintf(stderr, "Could not open file %s for writing\n", 
                outfile_name);
        exit(EXIT_FAILURE);
    }

    int dimensions[2];
    record_file_dimensions(infile, dimensions);
    int line_count = dimensions[0];
    int max_line_length = dimensions[1];
   
    char input_lines[line_count][max_line_length + 1]; 
    
    for (int i = 0; i < line_count; ++i) {
        char line[max_line_length + 1];
        if (fgets(line, sizeof(line), infile) != NULL) {
            strcpy(input_lines[i], line);
        }
    }
    // golfed:
    // for (int i = 0; fgets(input_lines[i], sizeof(input_lines[i]), infile); ++i);


    for (int i = line_count - 1; i >= 0; --i) {
        fprintf(outfile, "%s", input_lines[i]);
    }

    fclose(infile);
    fclose(outfile);

    return 0;
}

void record_file_dimensions(FILE *infile, int record[2]) {
    int line_count = 0;
    int max_line_length = 0;
    int this_line_length = 0;
    int c;
    while ((c = fgetc(infile)) != EOF) {
        ++this_line_length;
        if (c == '\n') {
            if (this_line_length > max_line_length) {
                max_line_length = this_line_length;
            }
            this_line_length = 0;
            ++line_count;
        }
    }
    rewind(infile);

    record[0] = line_count;
    record[1] = max_line_length;
} 

