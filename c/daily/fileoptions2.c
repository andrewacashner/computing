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
     int i, j, fields_found;

     struct {
	  char key[MAX_STR];
	  char value[MAX_STR];
     } setting[MAX_FIELDS];
     char *new_str;

     char *key_string[] = { "FirstName", "LastName", "Age", "Occupation", "E-mail" };
     enum { FIRSTNAME, LASTNAME, AGE, OCCUPATION, EMAIL } key_num;
     char firstname[MAX_STR], lastname[MAX_STR], age[MAX_STR], occupation[MAX_STR], email[MAX_STR];
     char *key_var[5];
     key_var[FIRSTNAME] = firstname;
     key_var[LASTNAME] = lastname;
     key_var[AGE] = age;
     key_var[OCCUPATION] = occupation;
     key_var[EMAIL] = email;

     
     if (argc != 2) {
	  fprintf(stderr, "Incorrect arguments. Usage: fileoptions <configuration file>\n");
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

     fields_found = i;
     for (i = 0; i < fields_found; ++i) {
	  for (j = 0; j < MAX_FIELDS; ++j) {
	       if (strcmp(setting[i].key, key_string[j]) == 0) {
		    strcpy(key_var[j], setting[i].value);
	       }
	  }
     }

     printf("Last name: %s, Occupation: %s.\n", lastname, occupation);
     
     fclose(configfile);

     return(0);
}

     
