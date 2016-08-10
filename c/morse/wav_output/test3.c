#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "wavfile.h"

#ifndef M_PI
  #define M_PI 3.14159265358979323846
#endif

#define MAX_FILENAME       80

#define NUM_SAMPLES       WAVFILE_SAMPLES_PER_SECOND / 14
#define UNIT_DURATION     NUM_SAMPLES
#define MAX_SIGNAL_LENGTH NUM_SAMPLES * 16

#define WRITE_DOT        write_tone(outfile, waveform, UNIT_DURATION)
#define WRITE_DASH       write_tone(outfile, waveform, 3 * UNIT_DURATION)
#define WRITE_SIGNAL_SPC write_silence(outfile, waveform, UNIT_DURATION);
#define WRITE_CHAR_SPC   write_silence(outfile, waveform, 2 * UNIT_DURATION);
#define WRITE_WORD_SPC   write_silence(outfile, waveform, 6 * UNIT_DURATION);


void write_tone(FILE *outfile, short waveform[], int duration);
void write_silence(FILE *outfile, short waveform[], int duration);
void morse_char(FILE *outfile, short waveform[], int signal_code[]);
void morse_word(FILE *outfile, short waveform[], int char_seq[], int length);

#define MAX_MORSE_CHARS 2
#define MAX_MORSE_CHAR_SEQ 5

enum { _dot, _dash, _endcode } signal_type;
int morse_alphabet[MAX_MORSE_CHARS][MAX_MORSE_CHAR_SEQ] = {
  {_dot, _dash, _endcode},
  {_dash, _dot, _dot, _dot, _endcode}
};
enum {_a, _b } morse_alpha_index;

int main(int argc, char *argv[])
{
  short waveform[MAX_SIGNAL_LENGTH];
  FILE *outfile;
  char outfile_name[MAX_FILENAME];
  int i;

  int message[2][5] = {
    { 4, _a, _b, _b, _a }, /* [0] = length */
    { 3, _b, _a, _a }
  };
  int message_length = 2;
  
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

  for (i = 0; i < message_length; ++i) {
    morse_word(outfile, waveform, &message[i][1], message[i][0]);
  }

  wavfile_close(outfile);

  return (0);
}


void write_tone(FILE *outfile, short waveform[], int duration)
{
  int i;
  double timepoint;
  double frequency = 800.0;
  int volume = 32000;
  for (i = 0; i < duration; ++i) {
    timepoint = (double) i / WAVFILE_SAMPLES_PER_SECOND;
    waveform[i] = volume * sin(frequency * timepoint * 2 * M_PI);
  }
  wavfile_write(outfile, waveform, duration);
  return;
}
void write_silence(FILE *outfile, short waveform[], int duration)
{
  int i;
  for (i = 0; i < duration; ++i) {
    waveform[i] = 0;
  }
  wavfile_write(outfile, waveform, duration);
  return;
}
void morse_char(FILE *outfile, short waveform[], int signal_code[])
{
  int i;
  
  for (i = 0; signal_code[i] != _endcode; ++i) {
    if (signal_code[i] == _dot) {
      WRITE_DOT;
    } else {
      WRITE_DASH;
    }
    WRITE_SIGNAL_SPC;
  }
  WRITE_CHAR_SPC;
  return;
}
void morse_word(FILE *outfile, short waveform[], int char_seq[], int length)
{
  int i;
  int this_char_index;
  for (i = 0; i < length; ++i) {
    this_char_index = char_seq[i];
    morse_char(outfile, waveform, morse_alphabet[this_char_index]);
  }
  WRITE_WORD_SPC;
  return;
}

  
