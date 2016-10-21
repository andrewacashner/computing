/* Read options from file;
   Write to stdin based on contents read 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STR    120
#define MAX_LINE   120
#define MAX_FIELDS 5

const char *key_value_separator = "=";
const char *field_separator = ";";

int main(int argc, char *argv[])
{
     FILE *configfile;
     char configfilename[MAX_STR];
     char line[MAX_LINE];
     int i, fields_found;

     struct {
	  char key[MAX_STR];
	  char value[MAX_STR];
     } setting[MAX_FIELDS];
     char *new_str;

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

     for (i = 0; i < MAX_FIELDS &&
	       fgets(line, sizeof(line), configfile) != NULL; ++i) {
	  
	  new_str = strtok(line, key_value_separator);
	  sscanf(new_str, "%s", setting[i].key);

	  new_str = strtok(NULL, field_separator);
	  sscanf(new_str, "%s", setting[i].value);
     }

     /* fields_found = i; */
     /* for (i = 0; i < fields_found; ++i) { */
     /* 	  fprintf(outfile, "| %d | %-*s | %-*s |\n", */
     /* 		  i, key_length, setting[i].key,  */
     /* 		  value_length, setting[i].value); */
     /* } */
     
     fclose(configfile);

     return(0);
}

     
