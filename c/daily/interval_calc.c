/* interval_calc.c -- Calculate musical interval above given pitch
 * Andrew Cashner, 2015-02-26
 *
 * Examples: 
 *  Input: C# + m3 --> Output: E
 *  	   Eb + d4 --> Output: AB (B = double flat)
 *         D  + M3 --> Output: F#
 *         A  + m3 --> Output: C
 *
 * ABBREVIATIONS
 * PC		pitch class
 * dia		diatonic
 * chrom	chromatic
 * perf		perfect interval
 * imperf	imperfect interval
 */

/****************************************************************************
 * TODO 
 * Set up input for interactive mode reading multiple entries from stdin
 * Allow more flexible input and output : C5 not Cn5, Cbb or CB, C## or CX
 * Optimize error tests
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_DIGITS 5
#define MAX_PITCHES 7
#define MAX_ACCIDENTALS 5
#define MAX_INTERVALS 5
#define MAX_PERFECTS 3
#define MAX_IMPERFECTS 4
#define PERF 0
#define IMPERF 1
#define TRUE 0
#define FALSE 1

/* Lookup tables */
static const char pitch_symbols[MAX_PITCHES]
		= {'C', 'D', 'E', 'F', 'G', 'A', 'B'};
static const int  pitch_values[MAX_PITCHES]	 
		= {  0,   2,   4,   5,   7,   9,  11}; 
static const char accidental_symbols[MAX_ACCIDENTALS] 
		= {'B', 'b', 'n', '#', 'X'};
static const int  accidental_values[MAX_ACCIDENTALS]  
		= { -2,  -1,   0,   1,   2};
static const char all_interval_symbols[MAX_INTERVALS] 
		= {'d', 'a', 'P', 'm', 'M'};
static const char perf_interval_symbols[MAX_PERFECTS]
		= {'d', 'P', 'a'};
static const int  perf_interval_values[MAX_PERFECTS] 
		= { -1,   0,   1};
static const char imperf_interval_symbols[MAX_IMPERFECTS]
		= {'d', 'm', 'M', 'a'};
static const int  imperf_interval_values[MAX_IMPERFECTS] 
		= { -2,  -1,   0,   1};
static const int perfect_imperfect_test[7] = 
		{PERF, IMPERF, IMPERF, PERF, PERF, IMPERF, IMPERF};


int main(int argc, char *argv[])
{
	char octave_str[MAX_DIGITS];
	char interval_quality;
	char interval_degree[MAX_DIGITS];
	char end_PC_letter;
	char end_PC_accidental;

	int i;	
	int start_PC_dia;
	int accidental_adjust;
	int start_octave;
	int extra_octave;
	int end_octave;
	int start_PC_chrom;
	int interval_dia;
	int interval_chrom;
	int operator;
	int interval_adjust;
	int end_PC_dia;
	int end_PC_chrom;
	int test; /* TRUE or FALSE */

/************************************************************/

	/* Check for valid command-line input, parse arguments, store values */
	/* ARG LENGTHS */
	if (strlen(argv[3]) > MAX_DIGITS || 
	    strlen(argv[2]) > 1 || strlen(argv[1]) > MAX_DIGITS) {
		fprintf(stderr, "Error: Incorrect input (usage instruction)\n");
		exit(EXIT_FAILURE);
	}
	/* PITCH CLASSES */
	if (argv[1][0] < 'A' || argv[1][0] > 'G') {
		fprintf(stderr, "Error: First character must be pitch class A-G\n");
		exit(EXIT_FAILURE);
	} else {
		/* Convert letter char to diatonic letter code 0--6 where C = 0 */
		start_PC_dia = (int)(argv[1][0] - 'C');
		if (start_PC_dia < 0) 
			start_PC_dia += 7; /* Rotate A and B to end */
	}


	/* ACCIDENTALS */
	for (i = 0, test = FALSE; i < MAX_ACCIDENTALS; i++) {
		if (argv[1][1] == accidental_symbols[i]) {
			test = TRUE;
			break;
		}
	}
	if (test == TRUE) {
		accidental_adjust = accidental_values[i];
	} else {
		fprintf(stderr, "Error: Second character must be accidental: "
			"B, b, n, #, or X\n");
		exit(EXIT_FAILURE);
	}

	/* INTERVALS */
	for (i = 0, test = FALSE; i < MAX_INTERVALS; i++) {
		if (argv[3][0] == all_interval_symbols[i]) {
			test = TRUE;
			break;
		}
	}
	if (test == TRUE) {
		interval_quality = argv[3][0];
	}
	else {
	    	fprintf(stderr, "Error: Interval quality must be "
			"d, a, P, m, or M\n");
		exit(EXIT_FAILURE);
	}

	/* Set operator to add or subtract (or error) */
	if (argv[2][0] == '+')
		operator = 1;
	else if (argv[2][0] == '-')
		operator = -1;
	else {
		fprintf(stderr, "Illegal operator: Use + or -\n");
		exit(EXIT_FAILURE);
	}

	/* Convert start octave string  and interval number string to integers */
	strcpy(octave_str, &argv[1][2]);
	sscanf(octave_str, "%d", &start_octave);
	
	/* Get chromatic PC code from diatonic PC code and accidental*/
	start_PC_chrom = start_octave * 12 + pitch_values[start_PC_dia] + accidental_adjust;
	
	/* Get diatonic interval; one less than given interval number */
	/* Adjust for intervals more than 7th */
	strcpy(interval_degree, &argv[3][1]);
	sscanf(interval_degree, "%d", &interval_dia);
	
	--interval_dia; 
	extra_octave = 0;
	if (interval_dia / 7 > 0) {
		extra_octave += interval_dia / 7;
		interval_dia %= 7;
	} 
	
	/* Get interval adjustment for chromatic interval from given quality */
	if (perfect_imperfect_test[interval_dia] == IMPERF) {
		for (i = 0, test = FALSE; i < MAX_IMPERFECTS; ++i) {
			if (imperf_interval_symbols[i] == interval_quality) {
				interval_adjust = imperf_interval_values[i];
				test = TRUE;
				break;
			}
		}
		if (test == FALSE) {
			fprintf(stderr, "Error: Imperfect intervals can only be "
				"d, m, M, or a\n");
			exit(EXIT_FAILURE);
		}
	} else {
		for (i = 0, test = FALSE; i < MAX_PERFECTS; ++i) {
			if (perf_interval_symbols[i] == interval_quality) {
				interval_adjust = perf_interval_values[i];
				test = TRUE;
				break;
			}
		}
		if (test == FALSE) {
			fprintf(stderr, "Error: Perfect intervals can only be "
				"d, P, or a\n");
			exit(EXIT_FAILURE);
		}
	}

	/* Get chromatic interval: chromatic equivalent of diatonic interval 
	 * 				+ adjustment just calculated + extra octaves
	 */
	interval_chrom = extra_octave * 12 + pitch_values[interval_dia] + interval_adjust;
	
	/* Get end chromatic pitch class */
	end_PC_chrom = start_PC_chrom + operator * interval_chrom;

	if (end_PC_chrom < 0) {
		fprintf(stderr, "Error: Result is lower than octave 0\n");
		exit(0);
	}

	/* Get base pitch class name 
	 * Check for octave overflow and adjust 
	 * Look up symbol
	 */
	end_PC_dia = start_PC_dia + interval_dia * operator;

	if (end_PC_dia < 0 || end_PC_dia > 6) {
		end_PC_dia -= 7 * operator;
	}

	end_PC_letter = pitch_symbols[end_PC_dia];

	/* Calculate accidental to add; look up symbol */
	accidental_adjust = end_PC_chrom % 12;
	accidental_adjust -= pitch_values[end_PC_dia];
	
	/* Make sure accidental_adjust is relative to nearest multiple of 12,
	 * -2 <= accidental_adjust <= 2 */
	if (accidental_adjust < -2) {
		accidental_adjust += 12;
	} else {
		if (accidental_adjust > 2) {
			accidental_adjust -= 12;
		}
	}
	
	/* In descending direction, add extra octave when crossing C boundary */
	if (operator < 0 && interval_chrom % 12 != 0) {
		++extra_octave;
	}
	end_octave = start_octave + operator * extra_octave;


#ifdef DEBUG	
	printf("start_PC_dia %d  \t| end_PC_dia %d\n", start_PC_dia, end_PC_dia);
	printf("start_PC_chrom %d\t| end_PC_chrom %d\n", start_PC_chrom, end_PC_chrom);
	printf("interval_dia %d  \t| interval_chrom %d\n", interval_dia, interval_chrom);
	printf("accidental_adjust %d\t| end_octave %d\n", accidental_adjust, end_octave);
#endif

	/* Get accidental symbol */
	for (i = 0; i < MAX_ACCIDENTALS 
		&& accidental_values[i] != accidental_adjust; ++i) 
		; /* Done */
	end_PC_accidental = accidental_symbols[i];

	/* OUTPUT RESULT */
	printf("%c%c%d\n", end_PC_letter, end_PC_accidental, end_octave);

	return(0);
}


