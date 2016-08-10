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


#define TONE volume*sin(frequency*t*2*M_PI)

#define DOT {						\
    for (i = 0; i < unit_duration; ++i) {		\
      t = (double) i / WAVFILE_SAMPLES_PER_SECOND;	\
      waveform[i] = TONE;				\
    }							\
    for (; i < (2 * unit_duration); ++i) {		\
      waveform[i] = 0;					\
    }							\
    wavfile_write(outfile,waveform,length);		\
  }

#define DASH {						\
    for (i = 0; i < (3 * unit_duration); ++i) {		\
      t = (double) i / WAVFILE_SAMPLES_PER_SECOND;	\
      waveform[i] = TONE;				\
    }							\
    for (; i < (4 * unit_duration); ++i) {		\
      waveform[i] = 0;					\
    }							\
    wavfile_write(outfile,waveform,length);		\
  }

#define SPACE {					\
    for (i = 0; i < (6 * unit_duration); ++i) {	\
      waveform[i] = 0;				\
    }						\
    wavfile_write(outfile,waveform,length);	\
  }

#define NUM_SAMPLES WAVFILE_SAMPLES_PER_SECOND / 16
#define DOT_DURATION NUM_SAMPLES * 2
#define DASH_DURATION NUM_SAMPLES * 4
#define SPACE_DURATION NUM_SAMPLES * 6
#define TOTAL_DURATION DOT_DURATION * 2 + DASH_DURATION * 2 + SPACE_DURATION

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

  DOT; DASH; SPACE;
  DASH; DOT;

  wavfile_close(outfile);

  return (0);
}

  
