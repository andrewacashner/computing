#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <errno.h>

#include "wavfile.h"

#ifndef M_PI
  #define M_PI 3.14159265358979323846
#endif

#define NUM_SAMPLES WAVFILE_SAMPLES_PER_SECOND / 8
#define TOTAL_DURATION NUM_SAMPLES*5

#define TONE volume*sin(frequency*t*2*M_PI)
#define DOT(START_I, SIGNAL_NUM) {\
  for (i = START_I; i < SIGNAL_NUM * unit_duration; ++i) {\
    t = (double) i / WAVFILE_SAMPLES_PER_SECOND;\
    waveform[i] = TONE;\
  }\
}
#define REST(START_I, SIGNAL_NUM) {\
    for (i = START_I; i < SIGNAL_NUM * unit_duration; ++i) {	\
      waveform[i] = 0;\
    }\
}
  

int main(void)
{
  short waveform[TOTAL_DURATION];
  double frequency = 800.0;
  int volume = 32000;
  int unit_duration = NUM_SAMPLES;
  int length = TOTAL_DURATION;
  FILE *outfile;
  char outfile_name[] = "test1.wav";
  double t;
  int i;

  
  outfile = wavfile_open(outfile_name);
  if (outfile == NULL) {
    printf("Could not open file %s for writing: %s\n", outfile_name, strerror(errno));
    exit(EXIT_FAILURE);
  }

  DOT(0, 1);
  REST(i, 2);
  DOT(i, 3);
  DOT(i, 4);
  DOT(i, 5);

  wavfile_write(outfile,waveform,length*3);
  wavfile_close(outfile);

  return (0);
}

  
