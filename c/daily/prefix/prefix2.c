#define FIRST "global"
#define SECOND "glossary"

#include<stdio.h>
#include<string.h>
int main()
{
  int i, prefix;
  char first[] = FIRST;
  char second[] = SECOND;

  if (first[0] == second[0]){
    for (i = prefix = 1; i < strlen(first) && i < strlen(second); ++i) {
      if (first[i] != second[i]) {
	first[i] = '\0';
	break;
      } 
    }
    printf("%s\n", first);
  }
  return(0);
}
    
    
