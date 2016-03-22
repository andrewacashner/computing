/* Replace all characters by a string of the characters <= that
   character of its type, c -> abc, C -> ABC, 3 -> 0123
   2015/10/27, for code golf
*/

#include<stdio.h>
int main(void)
{
  int c, i, n;
  char output;
  char *char_table[] = {"0Aa", "9Zz"};

  while ((c = getchar()) != EOF) {
    if (c < '0' || c > 'z') {
      putchar(c);
    } else {
      for (i = 0; i < 3; ++i) {
	if (c >= char_table[0][i] && c <= char_table[1][i]) {
	  n = c - char_table[0][i];
	  output = char_table[0][i];
	  break;
	}
      }
      for (i = 0; i <= n; ++i) {
	putchar(output);
	++output;
      }
    }
  }
  return(0);
}

      
      
			   
