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

/* For lookup tables */
const enum { DOT, DASH, CHAR_SPC, WORD_SPC, ENDCODE } sign_type;

const char *sign_output_str[MAX_OUTPUT_STR] = {
  ". ", "--- ", "  ", "      "
};


int main(int argc, char *argv[]) 
{

  int morse_table[MAX_CHARS][MAX_CHAR_SEQ + 1] = {
    {'A', DOT, DASH, ENDCODE },
    {'B', DASH, DOT, DOT, DOT, ENDCODE },
    {'C', DASH, DOT, DASH, DOT, ENDCODE },
    {'D', DASH, DOT, DOT, ENDCODE },
    {'E', DOT, ENDCODE },
    {'F', DOT, DOT, DASH, DOT, ENDCODE },
    {'G', DASH, DASH, DOT, ENDCODE },
    {'H', DOT, DOT, DOT, DOT, ENDCODE },
    {'I', DOT, DOT, ENDCODE },
    {'J', DOT, DASH, DASH, DASH, ENDCODE },
    {'K', DASH, DOT, DASH, ENDCODE },
    {'L', DOT, DASH, DOT, DOT, ENDCODE },
    {'M', DASH, DASH, ENDCODE },
    {'N', DASH, DOT, ENDCODE },
    {'0', DASH, DASH, DASH, ENDCODE },
    {'P', DOT, DASH, DASH, DOT, ENDCODE },
    {'Q', DASH, DASH, DOT, DASH, ENDCODE },
    {'R', DOT, DASH, DOT, ENDCODE },
    {'S', DOT, DOT, DOT, ENDCODE },
    {'T', DASH, ENDCODE },
    {'U', DOT, DOT, DASH, ENDCODE },
    {'V', DOT, DOT, DOT, DASH, ENDCODE },
    {'W', DOT, DASH, DASH, ENDCODE },
    {'X', DASH, DOT, DOT, DASH, ENDCODE },
    {'Y', DASH, DOT, DASH, DASH, ENDCODE },
    {'Z', DASH, DASH, DOT, DOT, ENDCODE },
    {'0', DASH, DASH, DASH, DASH, DASH, ENDCODE },
    {'1', DOT, DASH, DASH, DASH, DASH, ENDCODE },
    {'2', DOT, DOT, DASH, DASH, DASH, ENDCODE },
    {'3', DOT, DOT, DOT, DASH, DASH, ENDCODE },
    {'4', DOT, DOT, DOT, DOT, DASH, ENDCODE },
    {'5', DOT, DOT, DOT, DOT, DOT, ENDCODE },
    {'6', DASH, DOT, DOT, DOT, DOT, ENDCODE },
    {'7', DASH, DASH, DOT, DOT, DOT, ENDCODE },
    {'8', DASH, DASH, DASH, DOT, DOT, ENDCODE },
    {'9', DASH, DASH, DASH, DASH, DOT, ENDCODE },
    {'.', DOT, DASH, DOT, DASH, DOT, DASH, ENDCODE },
    {',', DASH, DASH, DOT, DOT, DASH, DASH, ENDCODE },
    {'?', DOT, DOT, DASH, DASH, DOT, DOT, ENDCODE },
    {'\'', DOT, DASH, DASH, DASH, DASH, DOT, ENDCODE },
    {'!', DASH, DOT, DASH, DOT, DASH, DASH, ENDCODE },
    {'/', DASH, DOT, DOT, DASH, DOT, ENDCODE },
    {'(', DASH, DOT, DASH, DASH, DOT, ENDCODE },
    {')', DASH, DOT, DASH, DASH, DOT, DASH, ENDCODE }, 
    {'&', DOT, DASH, DOT, DOT, DOT, ENDCODE },
    {':', DASH, DASH, DASH, DOT, DOT, DOT, ENDCODE },
    {';', DASH, DOT, DASH, DOT, DASH, DOT, ENDCODE },
    {'=', DASH, DOT, DOT, DOT, DASH, ENDCODE },
    {'+', DOT, DASH, DOT, DASH, DOT, ENDCODE },
    {'-', DASH, DOT, DOT, DOT, DOT, DASH, ENDCODE },
    {'_', DOT, DOT, DASH, DASH, DOT, DASH, ENDCODE },
    {'\"', DOT, DASH, DOT, DOT, DASH, DOT, ENDCODE },
    {'$', DOT, DOT, DOT, DASH, DOT, DOT, DASH, ENDCODE },
    {'@', DOT, DASH, DASH, DOT, DASH, DOT, ENDCODE },
    {' ', WORD_SPC, ENDCODE }
  };

  int ascii_table[MAX_ASCII];
  int morse_table_index;
  
  FILE *infile, *outfile;
  char infile_name[MAX_FILENAME], outfile_name[MAX_FILENAME];
  int i, ascii_char, sign_char_type;
  char output_str[MAX_OUTPUT_STR * MAX_CHAR_SEQ];

  /* Process options, open files for input and output from
     command-line arguments */
  
  if (argc != 3) {
    fprintf(stderr, "Incorrect number of arguments. "
            "Usage: txt2morse <input file> <output file>\n");
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

  /* Make lookup table to access morse codes through ascii values */
  for (i = 0; i < MAX_CHARS; ++i) {
    ascii_char = morse_table[i][0];
    ascii_table[ascii_char] = i;
  }
  
  /* Read in characters, look up series of dots and dashes in sign
     table, output appropriate format for each dot, dash, or space. */

  /** Need to convert to uppercase and ignore bad characters **/
  while ((ascii_char = fgetc(infile)) != EOF) {
    
    /* Build string for each morse character signal */
    output_str[0] = '\0';
    /* Get morse output patterns for each component character from
       lookup table, so 'A' -> DOT, DASH -> ". ---" */
    i = 1;
    while (1) {
      morse_table_index = ascii_table[ascii_char];
      sign_char_type = morse_table[morse_table_index][i];
      if (sign_char_type == ENDCODE) break;
      strcat(output_str, sign_output_str[sign_char_type]);
      ++i;
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


