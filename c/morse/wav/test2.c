#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "wavfile.h"

#ifndef M_PI
  #define M_PI 3.14159265358979323846
#endif

#define MAX_FILENAME 80

#define NUM_SAMPLES WAVFILE_SAMPLES_PER_SECOND / 14
#define MAX_SIGNAL_LENGTH NUM_SAMPLES * 16

#define WRITE_SILENCE(DURATION) {			\
    for (i = 0; i < DURATION; ++i) waveform[i] = 0;	\
    wavfile_write(outfile,waveform,DURATION);		\
  }
#define WRITE_TONE(DURATION) {					\
    for (i = 0; i < DURATION; ++i) {				\
      timing = (double) i/ WAVFILE_SAMPLES_PER_SECOND;		\
      waveform[i] = volume*sin(frequency*timing*2*M_PI);	\
    }								\
    wavfile_write(outfile,waveform,DURATION);			\
    SIGNAL_SPC;							\
  }
#define SIGNAL_SPC { WRITE_SILENCE(unit_duration); }
#define CHAR_SPC   { WRITE_SILENCE(2 * unit_duration); }
#define WORD_SPC   { WRITE_SILENCE(6 * unit_duration); }
#define DOT        { WRITE_TONE(unit_duration); }
#define DASH       { WRITE_TONE(3 * unit_duration); }

#define MORSE_CHAR(SIGNAL) { SIGNAL; CHAR_SPC; }
#define MORSE_WORD(CHAR)   { CHAR; WORD_SPC; }

#define MORSE_S MORSE_CHAR(DASH; DASH; DASH;);
#define MORSE_O MORSE_CHAR(DOT; DOT; DOT;);

int main(int argc, char *argv[])
{
  short waveform[MAX_SIGNAL_LENGTH];
  double frequency = 800.0;
  int volume = 32000;
  int unit_duration = NUM_SAMPLES;
  FILE *outfile;
  char outfile_name[MAX_FILENAME];
  double timing;
  int i;

  if (argc > 1) {
    fprintf(stderr, "Bad arguments\n");
    exit(EXIT_FAILURE);
  }
  strcpy(outfile_name, argv[0]);
  strcat(outfile_name, ".wav");
  
  outfile = wavfile_open(outfile_name);
  if (outfile == NULL) {
    fprintf(stderr, "Could not open file %s for writing\n", outfile_name);
    exit(EXIT_FAILURE);
  }


  MORSE_WORD(MORSE_CHAR(DOT; DASH;);  MORSE_CHAR(DASH; DOT; DOT; DOT;));

  MORSE_WORD(MORSE_S; MORSE_O; MORSE_S);

  wavfile_close(outfile);

  return (0);
}

  
