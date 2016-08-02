/* This is \.{txt2morse}, by Andrew A. Cashner, July 2016. */
/* The program reads in text from a file or standard input,  */
/* converts the text to morse code, and outputs the result */
/* to, potentially, a variety of formats. */

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define MAX_FILENAME 48
#define MAX_CHARS 55
#define MAX_CHAR_SEQ 8
#define MAX_OUTPUT_STR 24
#define MAX_ASCII 117

/* Data needed to create lookup tables */
const enum { EMPTY, DOT, DASH, CHAR_SPC, WORD_SPC } sign_type;

const char *sign_output_str[MAX_OUTPUT_STR] = {
  "[]", ". ", "--- ", "  ", "      "
};

const enum { ALPHA_NAME, ALPHA_LENGTH } alphabet_index;

int morse_table[MAX_CHARS][MAX_CHAR_SEQ + 2] = {
  {'A', 2, DOT, DASH },
  {'B', 4, DASH, DOT, DOT, DOT },
  {'C', 4, DASH, DOT, DASH, DOT },
  {'D', 3, DASH, DOT, DOT },
  {'E', 1, DOT },
  {'F', 4, DOT, DOT, DASH, DOT },
  {'G', 3, DASH, DASH, DOT },
  {'H', 4, DOT, DOT, DOT, DOT },
  {'I', 2, DOT, DOT },
  {'J', 4, DOT, DASH, DASH, DASH },
  {'K', 3, DASH, DOT, DASH },
  {'L', 4, DOT, DASH, DOT, DOT },
  {'M', 2, DASH, DASH },
  {'N', 2, DASH, DOT },
  {'0', 3, DASH, DASH, DASH },
  {'P', 4, DOT, DASH, DASH, DOT },
  {'Q', 4, DASH, DASH, DOT, DASH },
  {'R', 3, DOT, DASH, DOT },
  {'S', 3, DOT, DOT, DOT },
  {'T', 1, DASH },
  {'U', 3, DOT, DOT, DASH },
  {'V', 4, DOT, DOT, DOT, DASH },
  {'W', 3, DOT, DASH, DASH },
  {'X', 4, DASH, DOT, DOT, DASH },
  {'Y', 4, DASH, DOT, DASH, DASH },
  {'Z', 4, DASH, DASH, DOT, DOT },
  {'0', 5, DASH, DASH, DASH, DASH, DASH },
  {'1', 5, DOT, DASH, DASH, DASH, DASH },
  {'2', 5, DOT, DOT, DASH, DASH, DASH },
  {'3', 5, DOT, DOT, DOT, DASH, DASH },
  {'4', 5, DOT, DOT, DOT, DOT, DASH },
  {'5', 5, DOT, DOT, DOT, DOT, DOT },
  {'6', 5, DASH, DOT, DOT, DOT, DOT },
  {'7', 5, DASH, DASH, DOT, DOT, DOT },
  {'8', 5, DASH, DASH, DASH, DOT, DOT },
  {'9', 5, DASH, DASH, DASH, DASH, DOT },
  {'.', 6, DOT, DASH, DOT, DASH, DOT, DASH },
  {',', 6, DASH, DASH, DOT, DOT, DASH, DASH },
  {'?', 6, DOT, DOT, DASH, DASH, DOT, DOT },
  {'\'', 5, DOT, DASH, DASH, DASH, DASH, DOT },
  {'!', 6, DASH, DOT, DASH, DOT, DASH, DASH },
  {'/', 5, DASH, DOT, DOT, DASH, DOT },
  {'(', 5, DASH, DOT, DASH, DASH, DOT },
  {')', 6, DASH, DOT, DASH, DASH, DOT, DASH}, 
  {'&', 5, DOT, DASH, DOT, DOT, DOT },
  {':', 6, DASH, DASH, DASH, DOT, DOT, DOT },
  {';', 6, DASH, DOT, DASH, DOT, DASH, DOT },
  {'=', 5, DASH, DOT, DOT, DOT, DASH },
  {'+', 5, DOT, DASH, DOT, DASH, DOT },
  {'-', 6, DASH, DOT, DOT, DOT, DOT, DASH },
  {'_', 6, DOT, DOT, DASH, DASH, DOT, DASH },
  {'\"', 6, DOT, DASH, DOT, DOT, DASH, DOT },
  {'$', 7, DOT, DOT, DOT, DASH, DOT, DOT, DASH },
  {'@', 6, DOT, DASH, DASH, DOT, DASH, DOT },
  {' ', 1, WORD_SPC }
};

/* Need array of addresses matching ASCII number indices to above
   entries */

int main(int argc, char *argv[]) 
{

  FILE *infile, *outfile;
  char infile_name[MAX_FILENAME], outfile_name[MAX_FILENAME];
  int i, ascii_char, sign_char_type;
  char output_str[MAX_OUTPUT_STR * MAX_CHAR_SEQ];

  /* Process options, open files for input and output from
     command-line arguments */
  
  if (argc != 3) {
    fprintf(stderr, "Incorrect number of arguments. Usage: txt2morse <input file> <output file>\n");
    exit(EXIT_FAILURE);
  }
  strcpy(infile_name, argv[1]);
  infile = fopen(infile_name, "r");
  if (infile == NULL) {
    fprintf(stderr, "Could not open file %s for reading.\n", infile_name);
    exit(EXIT_FAILURE);
  }
  strcpy(outfile_name, argv[2]);
  outfile = fopen(outfile_name, "w");
  if (outfile == NULL) {
    fprintf(stderr, "Could not open file %s for writing.\n", outfile_name);
    exit(EXIT_FAILURE);
  }

  
  /* Read in characters, look up series of dots and dashes in sign
     table, output appropriate format for each dot, dash, or space. */

  /** Need to convert to uppercase and ignore bad characters **/
  while ((ascii_char = fgetc(infile)) != EOF) {
    output_str[0] = '\0';
    /* Build string for each morse character signal */
    for (i = 0; i < sign[ascii_char].length; ++i) {
      sign_char_type = sign[ascii_char].code[i];
      strcat(output_str, sign_output_str[sign_char_type]);
    }
    /* Add space between characters */
    strcat(output_str, sign_output_str[CHAR_SPC]); 
    fprintf(outfile, "%s", output_str);
  }
  fprintf(outfile, "\n");

  fclose(infile);
  fclose(outfile);
  return (0);
}




