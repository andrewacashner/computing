/* Command-line  metronome, Andrew Cashner, 2016/01/23 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define DEFAULT_BPM 60

void wait(double);

int main(int argc, char *argv[])
{
  int bpm;
  unsigned_int delay_seconds;
  switch (argc) {
  case (1):
    bpm = DEFAULT_BPM;
    break;
  case (2):
    sscanf(argv[1], "%d", &bpm);
    break;
  default:
    fprintf(stderr, "Usage: metronome <beats per minute>. If no bpm is specified, then default value of 60 is used.\n");
    return(EXIT_FAILURE);
  }
  
  delay_seconds = (60 / bpm);
  printf("%.0f bpm = %.2f second delay\n", bpm, delay_seconds);
  while (1) {
    wait(delay_seconds);
    printf("o ");
  }
  
  return(0);
}

void wait(double wait_seconds)
{
  /* Save start clock tick */
  const clock_t start = clock();
  
  clock_t current;
  do {
    /* Get current clock tick */
    current = clock();

    /* Break loop when the requested number of seconds have elapsed */
  } while((double)(current - start) / CLOCKS_PER_SEC);
  
  return;
}
