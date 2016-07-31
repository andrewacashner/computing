/*1:*/
#line 6 "./txt2morse.w"

#include "stdio.h"
#include "string.h"

/*2:*/
#line 20 "./txt2morse.w"


#define MAX_CHARS 54
#define MAX_CHAR_SEQ 8
#define MAX_OUTPUT_STR 24

enum{DOT,DASH,CHAR_SPC,WORD_SPC}sign_type;

char*sign_output_str[MAX_OUTPUT_STR]= {
". ","--- ","  ","      "
}
;
struct{
int length;
int code[MAX_CHAR_SEQ];
}sign[MAX_CHARS];

int char_code[MAX_CHARS][MAX_CHAR_LENGTH]= {
{DOT,DASH},
{DASH,DOT,DOT,DOT},
{DASH,DOT,DASH,DOT},
{DASH,DOT,DOT},
{DOT},
{DOT,DOT,DASH,DOT},
{DASH,DASH,DOT},
{DOT,DOT,DOT,DOT},
{DOT,DOT},
{DOT,DASH,DASH,DASH},
{DASH,DOT,DASH},
{DOT,DASH,DOT,DOT},
{DASH,DASH},
{DASH,DOT},
{DASH,DASH,DASH},
{DOT,DASH,DASH,DOT},
{DASH,DASH,DOT,DASH},
{DOT,DASH,DOT},
{DOT,DOT,DOT},
{DASH},
{DOT,DOT,DASH},
{DOT,DOT,DOT,DASH},
{DOT,DASH,DASH},
{DASH,DOT,DOT,DASH},
{DASH,DOT,DASH,DASH},
{DASH,DASH,DOT,DOT},
{DASH,DASH,DASH,DASH,DASH},
{DOT,DASH,DASH,DASH,DASH},
{DOT,DOT,DASH,DASH,DASH},
{DOT,DOT,DOT,DASH,DASH},
{DOT,DOT,DOT,DOT,DASH},
{DOT,DOT,DOT,DOT,DOT},
{DASH,DOT,DOT,DOT,DOT},
{DASH,DASH,DOT,DOT,DOT},
{DASH,DASH,DASH,DOT,DOT},
{DASH,DASH,DASH,DASH,DOT},
{DOT,DASH,DOT,DASH,DOT,DASH},
{DASH,DASH,DOT,DOT,DASH,DASH},
{DOT,DOT,DASH,DASH,DOT,DOT},
{DOT,DASH,DASH,DASH,DASH,DOT},
{DASH,DOT,DASH,DOT,DASH,DASH},
{DASH,DOT,DOT,DASH,DOT},
{DASH,DOT,DASH,DASH,DOT},
{DASH,DOT,DASH,DASH,DOT,DASH},
{DOT,DASH,DOT,DOT,DOT},
{DASH,DASH,DASH,DOT,DOT,DOT},
{DASH,DOT,DASH,DOT,DASH,DOT},
{DASH,DOT,DOT,DOT,DASH},
{DOT,DASH,DOT,DASH,DOT},
{DASH,DOT,DOT,DOT,DOT,DASH},
{DOT,DOT,DASH,DASH,DOT,DASH},
{DOT,DASH,DOT,DOT,DASH,DOT},
{DOT,DOT,DOT,DASH,DOT,DOT,DASH},
{DOT,DASH,DASH,DOT,DASH,DOT}
};

enum{
A_CH,B_CH,C_CH,D_CH,E_CH,F_CH,G_CH,H_CH,
I_CH,J_CH,K_CH,L_CH,M_CH,N_CH,O_CH,P_CH,
Q_CH,R_CH,S_CH,T_CH,U_CH,V_CH,W_CH,X_CH,Y_CH,Z_CH,
ZERO,ONE,TWO,THREE,FOUR,FIVE,SIX,
SEVEN,EIGHT,NINE,
PERIOD,COMMA,QUESTION,APOSTROPHE,
EXCLAMATION,SLASH,START_PAREN,END_PAREN,
AMPERSAND,COLON,SEMICOLON,EQUALS,PLUS,
MINUS,UNDERSCORE,QUOTE,DOLLAR,AT
}char_index;

void new_sign(char ascii_char,int this_length,int this_code[])
{
int i;
sign[ascii_char].length= this_length;
for(i= 0;i,this.length;++i){
sign[ascii_char].code[i]= this_code[i];
}
return;
}

/*:2*/
#line 10 "./txt2morse.w"
;

int main(int argc,char*argv[])
{
/*3:*/
#line 118 "./txt2morse.w"

int ascii_char,i,sign_char_type;
char output_str[MAX_OUTPUT_STR*MAX_CHAR_SEQ];

while((ascii_char= fgetc(infile))!=0){
output_str[0]= '\0';
for(i= 0;i<sign[ascii_char].length;++i){
sign_char_type= sign[ascii_char].code[i];
strcat(output_str,sign_output_str[sign_char_type]);
}
}
/*:3*/
#line 14 "./txt2morse.w"
;
return(0);
}

/*:1*/
