/* Read a file and output the file with all the characters in reverse order */
#include <stdio.h>
#include <stdlib.h>
#define MAXBUF 256

int main(int argc, char *argv[])
{
  FILE *infile;
  FILE *outfile;
  int c, i, j, size;
  int inbuffer[MAXBUF], outbuffer[MAXBUF];
  
  if (argc != 3) {
    fprintf(stderr, "Usage: backwards <input file> <output file>\n");
    exit(EXIT_FAILURE);
  }

  infile = fopen(argv[1], "rb");
  if (infile == NULL) {
    fprintf(stderr, "Could not open file %s for reading.\n", argv[1]);
    exit(EXIT_FAILURE);
  }
  outfile = fopen(argv[2], "wb");
  if (outfile == NULL) {
    fprintf(stderr, "Could not open file %s for writing\n", argv[2]);
    exit(EXIT_FAILURE);
  }

  /* Copy input file into input buffer */
  for (i = 0; ((c = fgetc(infile)) != EOF); ++i) {
    inbuffer[i] = c;
  }
  size = i + 1;
  
  /* Copy characters in reverse to output buffer*/
  for (j = 0; j < size; ++j) {
    outbuffer[j] = inbuffer[i];
    --i;
  }
  
  /* Write output buffer to outfile */
  for (i = 0; i < size; ++i) {
    fputc(outbuffer[i], outfile);
  }

  /* Close files */
  fclose(infile);
  fclose(outfile);

  return(0);
}

  
    
  
  
