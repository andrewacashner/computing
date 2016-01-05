#include <stdio.h>
int main(void)
{
  char vowels[] = "aeiouAEIOU";
  enum {Normal, Vowel} mode = Normal;
  int c, i;
  char last;
  while ((c = getchar()) != EOF) {
    if (mode == Normal) {
      for (i = 0; i < 10; ++i) {
	if (vowels[i] == (char)c) {
	  last = (char)c;
	    mode = Vowel;
	    break;
	}
      }
      putchar(c);
    } else {
      if (last == (char)c) {
	continue;
      } else {
	mode = Normal;
	for (i = 0; i < 10; ++i) {
	  if (vowels[i] == (char)c) {
	    last = (char)c;
	    mode = Vowel;
	    break;
	  }
	}
	putchar(c);
      }
    }
  }

  return(0);
}
