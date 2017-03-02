/* Write and read options from file */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STR    120
#define MAX_LINE   120
#define MAX_FIELDS 5

#define PRINT_HLINE {				\
	  for (i = 0; i < line_length; ++i) {	\
	       fprintf(outfile, "-");		\
	  }					\
	  fprintf(outfile, "\n"); }
     

const char *key_value_separator = "=";
const char *field_separator = ";";

void trimspaces(char *string);

int main(int argc, char *argv[])
{
     FILE *configfile, *outfile;
     char configfilename[MAX_STR], outfilename[MAX_STR];
     char line[MAX_LINE];
     int i, fields_found;

     struct {
	  char key[MAX_STR];
	  char value[MAX_STR];
     } setting[MAX_FIELDS];
     char *new_str;

     int test_length, key_length, value_length, line_length;
     test_length = key_length = value_length = 0;
     
     if (argc != 3) {
	  fprintf(stderr, "Incorrect arguments. Usage: fileoptions <configuration file> <output file>\n");
	  exit(EXIT_FAILURE);
     }

     strcpy(configfilename, argv[1]);
     configfile = fopen(configfilename, "r");
     if (configfile == NULL) {
	  fprintf(stderr, "Could not open file %s for reading.\n", configfilename);
	  exit(EXIT_FAILURE);
     }

     strcpy(outfilename, argv[2]);
     outfile = fopen(outfilename, "w");
     if (outfile == NULL) {
	  fprintf(stderr, "Could not open file %s for writing.\n", outfilename);
	  exit(EXIT_FAILURE);
     }

     for (i = 0; i < MAX_FIELDS &&
	       fgets(line, sizeof(line), configfile) != NULL; ++i) {
	  
	  new_str = strtok(line, key_value_separator);
	  sscanf(new_str, "%s", setting[i].key);

	  test_length = strlen(setting[i].key);
	  if (test_length > key_length) {
	       key_length = test_length;
	  }
	  
	  new_str = strtok(NULL, field_separator);
	  sscanf(new_str, "%s", setting[i].value);

	  test_length = strlen(setting[i].value);
	  if (test_length > value_length) {
	       value_length = test_length;
	  }
     }


     fields_found = i;
     line_length = 11 + MAX_FIELDS / 10 + key_length + value_length;

     PRINT_HLINE;
     for (i = 0; i < fields_found; ++i) {
	  fprintf(outfile, "| %d | %-*s | %-*s |\n",
		  i, key_length, setting[i].key, 
		  value_length, setting[i].value);
     }
     PRINT_HLINE;
     
     fclose(configfile);
     fclose(outfile);

     return(0);
}

     
