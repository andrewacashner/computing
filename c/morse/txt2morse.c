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

/* Data needed to create lookup tables */
const enum { DOT, DASH, CHAR_SPC, WORD_SPC } sign_type;

const char *sign_output_str[MAX_OUTPUT_STR] = {
  ". ", "--- ", "  ", "      "
};

const enum { ALPHA_NAME, ALPHA_LENGTH } alphabet_index;

int alphabet[MAX_CHARS][MAX_CHAR_SEQ] = {
  {'A', 2},  {'B', 4},  {'C', 4},  {'D', 3},  {'E', 1},  {'F', 4},
  {'G', 3},  {'H', 4},  {'I', 2},  {'J', 4},  {'K', 3},  {'L', 4},
  {'M', 2},  {'N', 2},  {'0', 3},  {'P', 4},  {'Q', 4},  {'R', 3},
  {'S', 3},  {'T', 1},  {'U', 3},  {'V', 4},  {'W', 3},  {'X', 4},
  {'Y', 4},  {'Z', 4},  {'0', 5},  {'1', 5},  {'2', 5},  {'3', 5},
  {'4', 5},  {'5', 5},  {'6', 5},  {'7', 5},  {'8', 5},  {'9', 5},
  {'.', 6},  {',', 6},  {'?', 6},  {'\'', 5},  {'!', 6},  {'/', 5},
  {'(', 5},  {')', 6},  {'&', 5},  {':', 6},  {';', 6},  {'=', 5},
  {'+', 5},  {'-', 6},  {'_', 6},  {'\"', 6},  {'$', 7},  {'@', 6},
  {' ', 1}
};

int char_code[MAX_CHARS][MAX_CHAR_SEQ] = { 
   /* A */ { DOT, DASH }, 
   /* B */ { DASH, DOT, DOT, DOT },
   /* C */ { DASH, DOT, DASH, DOT },
   /* D */ { DASH, DOT, DOT },
   /* E */ { DOT },
   /* F */ { DOT, DOT, DASH, DOT },
   /* G */ { DASH, DASH, DOT },
   /* H */ { DOT, DOT, DOT, DOT },
   /* I */ { DOT, DOT },
   /* J */ { DOT, DASH, DASH, DASH },
   /* K */ { DASH, DOT, DASH },
   /* L */ { DOT, DASH, DOT, DOT },
   /* M */ { DASH, DASH },
   /* N */ { DASH, DOT },
   /* O */ { DASH, DASH, DASH },
   /* P */ { DOT, DASH, DASH, DOT },
   /* Q */ { DASH, DASH, DOT, DASH },
   /* R */ { DOT, DASH, DOT },
   /* S */ { DOT, DOT, DOT },
   /* T */ { DASH },
   /* U */ { DOT, DOT, DASH },
   /* V */ { DOT, DOT, DOT, DASH },
   /* W */ { DOT, DASH, DASH },
   /* X */ { DASH, DOT, DOT, DASH },
   /* Y */ { DASH, DOT, DASH, DASH },
   /* Z */ { DASH, DASH, DOT, DOT },
   /* 0 */ { DASH, DASH, DASH, DASH, DASH },
   /* 1 */ { DOT, DASH, DASH, DASH, DASH },
   /* 2 */ { DOT, DOT, DASH, DASH, DASH },
   /* 3 */ { DOT, DOT, DOT, DASH, DASH },
   /* 4 */ { DOT, DOT, DOT, DOT, DASH },
   /* 5 */ { DOT, DOT, DOT, DOT, DOT },
   /* 6 */ { DASH, DOT, DOT, DOT, DOT },
   /* 7 */ { DASH, DASH, DOT, DOT, DOT },
   /* 8 */ { DASH, DASH, DASH, DOT, DOT },
   /* 9 */ { DASH, DASH, DASH, DASH, DOT },
   /* . */ { DOT, DASH, DOT, DASH, DOT, DASH },
   /* , */ { DASH, DASH, DOT, DOT, DASH, DASH },
   /* ? */ { DOT, DOT, DASH, DASH, DOT, DOT },
   /* ' */ { DOT, DASH, DASH, DASH, DASH, DOT },
   /* ! */ { DASH, DOT, DASH, DOT, DASH, DASH },
   /* / */ { DASH, DOT, DOT, DASH, DOT },
   /* ( */ { DASH, DOT, DASH, DASH, DOT },
   /* ) */ { DASH, DOT, DASH, DASH, DOT, DASH}, 
   /* & */ { DOT, DASH, DOT, DOT, DOT },
   /* : */ { DASH, DASH, DASH, DOT, DOT, DOT },
   /* ; */ { DASH, DOT, DASH, DOT, DASH, DOT },
   /* = */ { DASH, DOT, DOT, DOT, DASH },
   /* + */ { DOT, DASH, DOT, DASH, DOT },
   /* - */ { DASH, DOT, DOT, DOT, DOT, DASH },
   /* _ */ { DOT, DOT, DASH, DASH, DOT, DASH },
   /* " */ { DOT, DASH, DOT, DOT, DASH, DOT },
   /* $ */ { DOT, DOT, DOT, DASH, DOT, DOT, DASH },
   /* @ */ { DOT, DASH, DASH, DOT, DASH, DOT },
   /* [space] */ { WORD_SPC }
};

struct {
  int length;
  int code[MAX_CHAR_SEQ];
} sign[MAX_CHARS];

void new_sign(int ascii_char, int this_length, int this_code[]);

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

  /* Build lookup table */
  for (i = 0; i < MAX_CHARS; ++i) {
    new_sign(alphabet[i][ALPHA_NAME], alphabet[i][ALPHA_LENGTH], char_code[i]);
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

void new_sign(int ascii_char, int this_length, int this_code[])
/* Add a new entry in the lookup table (struct sign[]), keyed to the
   ASCII code */
{
  int i;
  sign[ascii_char].length = this_length;
  for (i = 0; i < this_length; ++i ) {
    sign[ascii_char].code[i] = this_code[i];
  }
  return;
}


