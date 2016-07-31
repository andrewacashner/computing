@* Introduction.
This is \.{txt2morse}. 
The program reads in text from a file or standard input, converts the text to morse code, and outputs the result to a variety of formats.
Written by Andrew A. Cashner, July 2016.

@c
#include "stdio.h"
#include "string.h"

@<Define values for lookup tables@>;

int main(int argc, char *argv[]) 
{
	@<Build lookup tables@>;
	return (0);
}

@ Teach the computer morse code.

@<Define values for lookup tables@>=

#define MAX_CHARS 54
#define MAX_CHAR_SEQ 8
#define MAX_OUTPUT_STR 24

enum { DOT, DASH, CHAR_SPC, WORD_SPC } sign_type;

char *sign_output_str[MAX_OUTPUT_STR] = {
	". ", "--- ", "  ", "      "
}
;
struct {
	int length;
	int code[MAX_CHAR_SEQ];
} sign[MAX_CHARS];

int char_code[MAX_CHARS][MAX_CHAR_LENGTH] = { 
	@| /* A */ { DOT, DASH }, 
	@| /* B */ { DASH, DOT, DOT, DOT },
	@| /* C */ { DASH, DOT, DASH, DOT },
	@| /* D */ { DASH, DOT, DOT },
	@| /* E */ { DOT },
	@| /* F */ { DOT, DOT, DASH, DOT },
	@| /* G */ { DASH, DASH, DOT },
	@| /* H */ { DOT, DOT, DOT, DOT },
	@| /* I */ { DOT, DOT },
	@| /* J */ { DOT, DASH, DASH, DASH },
	@| /* K */ { DASH, DOT, DASH },
	@| /* L */ { DOT, DASH, DOT, DOT },
	@| /* M */ { DASH, DASH },
	@| /* N */ { DASH, DOT },
	@| /* O */ { DASH, DASH, DASH },
	@| /* P */ { DOT, DASH, DASH, DOT },
	@| /* Q */ { DASH, DASH, DOT, DASH },
	@| /* R */ { DOT, DASH, DOT },
	@| /* S */ { DOT, DOT, DOT },
	@| /* T */ { DASH },
	@| /* U */ { DOT, DOT, DASH },
	@| /* V */ { DOT, DOT, DOT, DASH },
	@| /* W */ { DOT, DASH, DASH },
	@| /* X */ { DASH, DOT, DOT, DASH },
	@| /* Y */ { DASH, DOT, DASH, DASH },
	@| /* Z */ { DASH, DASH, DOT, DOT },
	@| /* 0 */ { DASH, DASH, DASH, DASH, DASH },
	@| /* 1 */ { DOT, DASH, DASH, DASH, DASH },
	@| /* 2 */ { DOT, DOT, DASH, DASH, DASH },
	@| /* 3 */ { DOT, DOT, DOT, DASH, DASH },
	@| /* 4 */ { DOT, DOT, DOT, DOT, DASH },
	@| /* 5 */ { DOT, DOT, DOT, DOT, DOT },
	@| /* 6 */ { DASH, DOT, DOT, DOT, DOT },
	@| /* 7 */ { DASH, DASH, DOT, DOT, DOT },
	@| /* 8 */ { DASH, DASH, DASH, DOT, DOT },
	@| /* 9 */ { DASH, DASH, DASH, DASH, DOT },
	@| /* . */ { DOT, DASH, DOT, DASH, DOT, DASH },
	@| /* , */ { DASH, DASH, DOT, DOT, DASH, DASH },
	@| /* ? */ { DOT, DOT, DASH, DASH, DOT, DOT },
	@| /* ' */ { DOT, DASH, DASH, DASH, DASH, DOT },
	@| /* ! */ { DASH, DOT, DASH, DOT, DASH, DASH },
	@| /* / */ { DASH, DOT, DOT, DASH, DOT },
	@| /* ( */ { DASH, DOT, DASH, DASH, DOT },
	@| /* ) */ { DASH, DOT, DASH, DASH, DOT, DASH}, 
	@| /* ampersand */ { DOT, DASH, DOT, DOT, DOT },
	@| /* : */ { DASH, DASH, DASH, DOT, DOT, DOT },
	@| /* ; */ { DASH, DOT, DASH, DOT, DASH, DOT },
	@| /* = */ { DASH, DOT, DOT, DOT, DASH },
	@| /* + */ { DOT, DASH, DOT, DASH, DOT },
	@| /* - */ { DASH, DOT, DOT, DOT, DOT, DASH },
	@| /* underscore */ { DOT, DOT, DASH, DASH, DOT, DASH },
	@| /* " */ { DOT, DASH, DOT, DOT, DASH, DOT },
	@| /* \$ */ { DOT, DOT, DOT, DASH, DOT, DOT, DASH },
	@| /* @@ */ { DOT, DASH, DASH, DOT, DASH, DOT }
};

enum { 
	A_CH, B_CH, C_CH, D_CH, E_CH, F_CH, G_CH, H_CH, 
	I_CH, J_CH, K_CH, L_CH, M_CH, N_CH, O_CH, P_CH, 
	Q_CH, R_CH, S_CH, T_CH, U_CH, V_CH, W_CH, X_CH, Y_CH, Z_CH,
	ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, 
	SEVEN, EIGHT, NINE, 
	PERIOD, COMMA, QUESTION, APOSTROPHE,
	EXCLAMATION, SLASH, START_PAREN, END_PAREN,
	AMPERSAND, COLON, SEMICOLON, EQUALS, PLUS,
	MINUS, UNDERSCORE, QUOTE, DOLLAR, AT
} char_index;

void new_sign(char ascii_char, int this_length, int this_code[])
{
	int i;
	sign[ascii_char].length = this_length;
	for (i = 0; i , this.length; ++i ) {
		sign[ascii_char].code[i] = this_code[i];
	}
	return;
}

@ Now we put it together to build to the lookup tables.

@<Build lookup tables@>=
int ascii_char, i, sign_char_type;
char output_str[MAX_OUTPUT_STR * MAX_CHAR_SEQ];

while ((ascii_char = fgetc(infile)) != 0) {
	output_str[0] = '\0';
	for (i = 0; i < sign[ascii_char].length; ++i) {
		sign_char_type = sign[ascii_char].code[i];
		strcat(output_str, sign_output_str[sign_char_type]);
	}
}

