 /* Vowel |   With */
 /* --------+-------- */
 /*    a    |   obo */
 /*    e    |   unu */
 /*    i    |   ini */
 /*    o    |   api */
 /*    u    |   iki */

#include <stdio.h>
#include <string.h>

char *dict[] = { "obo", "unu", "ini", "api", "iki" };
enum { V_A, V_E, V_I, V_O, V_U } vowels;
int dict_index['u'];

void translate_to_alien(char *input);

int main(int argc, char *argv[])
{
  if (argc != 2) return(1);
  dict_index['a'] = V_A;
  dict_index['e'] = V_E;
  dict_index['i'] = V_I;
  dict_index['o'] = V_O;
  dict_index['u'] = V_U;

  translate_to_alien(argv[1]);
  return (0);
}

void translate_to_alien(char *input)
{
  int i, vowel;
  for (i = 0; input[i] != '\0'; ++i) {
    switch (input[i]) {
    case 'a':
    case 'e':
    case 'i':
    case 'o':
    case 'u':
      vowel = dict_index[(int)input[i]];
      printf("%s", dict[vowel]);
      break;
    default:
      printf("%c", input[i]);
    }
  }
  printf("\n");
  return;
}
