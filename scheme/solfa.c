/* TODO deal with blank values, get order of hexachords right */
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

enum { 
    ut, re, mi, fa, sol, la, scale_max
} scale_symbol;

#define blank scale_max

char *scalenames[] = {
    "ut", "re", "mi", "fa", "sol", "la", ""
};

enum { 
    p_C, p_D, p_E, p_F, p_G, p_A, p_B_mol, p_B_dur, p_max
} pitch_symbol;

char *pitchnames[] = { 
    "C", "D", "E", "F", "G", "A", "B-mol", "B-dur", ""
};

enum { nat = 1, dur, mol, hex_max } hexachord_symbols; /* index to gamut */

char *hexachords[] = { "", "nat", "dur", "mol", "" };

int gamut[][4] = {
    { p_C, sol, fa, ut },
    { p_D, la, sol, re },
    { p_E, blank, la, mi },
    { p_F, ut, blank, fa },
    { p_G, re, ut, sol },
    { p_A, mi, re, la },
    { p_B_mol, fa, blank, blank },
    { p_B_dur, blank, mi, blank }
};


/* Function prototypes */
int pitch_to_hexachord(int pitch, int hexachord);
int member_index(char *key, char **array, int max);


int main(int argc, char *argv[]) {

    char *lettername, *hex_name;
    int letter_symbol, hex_symbol;
    char *solfa;
    int solfa_symbol;
    
    if (argc != 3) {
        exit(EXIT_FAILURE);
    } 
    lettername = argv[1];
    hex_name = argv[2];

    letter_symbol = member_index(lettername, pitchnames, p_max);
    if (letter_symbol >= p_max) { 
        exit(EXIT_FAILURE);
    } 

    hex_symbol = member_index(hex_name, hexachords, hex_max);
    if (hex_symbol >= hex_max ) {
        exit(EXIT_FAILURE);
    }
       
    solfa_symbol = pitch_to_hexachord(letter_symbol, hex_symbol);
    solfa = scalenames[solfa_symbol];

    printf("%s\n", solfa);
    
    return(0);
}

/* Functions */
int member_index(char *key, char **array, int max) {
    int i, result;
    for (i = 0; i < max; ++i) {
        if (strcmp(key, array[i]) == 0) {
            result = i;
            break;
        }
    }
    return(result);
}

int pitch_to_hexachord(int pitch, int hexachord) {
    
    assert(pitch >= p_C && pitch <= p_B_dur);
    assert(hexachord >= nat && hexachord <= mol);

    return(gamut[pitch][hexachord]);
}
