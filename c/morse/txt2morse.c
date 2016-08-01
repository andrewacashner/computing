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

const enum { 
  A_CH, B_CH, C_CH, D_CH, E_CH, F_CH, G_CH, H_CH, 
  I_CH, J_CH, K_CH, L_CH, M_CH, N_CH, O_CH, P_CH, 
  Q_CH, R_CH, S_CH, T_CH, U_CH, V_CH, W_CH, X_CH, Y_CH, Z_CH,
  ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, 
  SEVEN, EIGHT, NINE, 
  PERIOD, COMMA, QUESTION, APOSTROPHE,
  EXCLAMATION, SLASH, START_PAREN, END_PAREN,
  AMPERSAND, COLON, SEMICOLON, EQUALS, PLUS,
  MINUS, UNDERSCORE, QUOTE, DOLLAR, AT, SPACE_CH
} char_index;

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
  new_sign('A', 2, char_code[A_CH]);
  new_sign('B', 4, char_code[B_CH]);
  new_sign('C', 4, char_code[C_CH]);
  new_sign('D', 3, char_code[D_CH]);
  new_sign('E', 1, char_code[E_CH]);
  new_sign('F', 4, char_code[F_CH]);
  new_sign('G', 3, char_code[G_CH]);
  new_sign('H', 4, char_code[H_CH]);
  new_sign('I', 2, char_code[I_CH]);
  new_sign('J', 4, char_code[J_CH]);
  new_sign('K', 3, char_code[K_CH]);
  new_sign('L', 4, char_code[L_CH]);
  new_sign('M', 2, char_code[M_CH]);
  new_sign('N', 2, char_code[N_CH]);
  new_sign('O', 3, char_code[O_CH]);
  new_sign('P', 4, char_code[P_CH]);
  new_sign('Q', 4, char_code[Q_CH]);
  new_sign('R', 3, char_code[R_CH]);
  new_sign('S', 3, char_code[S_CH]);
  new_sign('T', 1, char_code[T_CH]);
  new_sign('U', 3, char_code[U_CH]);
  new_sign('V', 4, char_code[V_CH]);
  new_sign('W', 3, char_code[W_CH]);
  new_sign('X', 4, char_code[X_CH]);
  new_sign('Y', 4, char_code[Y_CH]);
  new_sign('Z', 4, char_code[Z_CH]);
  new_sign('0', 5, char_code[ZERO]);
  new_sign('1', 5, char_code[ONE]);
  new_sign('2', 5, char_code[TWO]);
  new_sign('3', 5, char_code[THREE]);
  new_sign('4', 5, char_code[FOUR]);
  new_sign('5', 5, char_code[FIVE]);
  new_sign('6', 5, char_code[SIX]);
  new_sign('7', 5, char_code[SEVEN]);
  new_sign('8', 5, char_code[EIGHT]);
  new_sign('9', 5, char_code[NINE]);
  new_sign('.', 6, char_code[PERIOD]);
  new_sign(',', 6, char_code[COMMA]);
  new_sign('?', 6, char_code[QUESTION]);
  new_sign('\'', 5, char_code[APOSTROPHE]);
  new_sign('!', 6, char_code[EXCLAMATION]);
  new_sign('/', 5, char_code[SLASH]);
  new_sign('(', 5, char_code[START_PAREN]);
  new_sign(')', 6, char_code[END_PAREN]);
  new_sign('&', 5, char_code[AMPERSAND]);
  new_sign(':', 6, char_code[COLON]);
  new_sign(';', 6, char_code[SEMICOLON]);
  new_sign('=', 5, char_code[EQUALS]);
  new_sign('+', 5, char_code[PLUS]);
  new_sign('-', 6, char_code[MINUS]);
  new_sign('_', 6, char_code[UNDERSCORE]);
  new_sign('\"', 6, char_code[QUOTE]);
  new_sign('$', 7, char_code[DOLLAR]);
  new_sign('@', 6, char_code[AT]);
  new_sign(' ', 1, char_code[SPACE_CH]);

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


