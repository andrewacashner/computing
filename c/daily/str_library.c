#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define MAX_CHAR 16

uint8_t digits(uint8_t num)
{
  uint8_t digits = 0;
  for (uint8_t rem = num; rem > 0; rem /= 10)
  {
    ++digits;
  }
  return digits;
}

void str_write_num(char *buffer, uint8_t num)
{
  char stack[digits(num)];
  uint8_t stack_index = 0;
  for (uint8_t digit = num; digit > 0; digit /= 10)
  {
    //  printf("Saving '%c' to stack index %d\n", '0' + digit % 10, stack_index);
    stack[stack_index] = '0' + digit % 10;
    ++stack_index;
  }

  --stack_index;

  uint8_t write_index;
  for (write_index = 0; write_index <= stack_index; ++write_index)
  {
    buffer[write_index] = stack[stack_index - write_index];
  }

  buffer[write_index] = '\0';
}

// NB assuming str1 is big enough for both
void str_cat(char *str1, char *str2)
{
  char *str1_ptr;
  for (str1_ptr = str1; *str1_ptr != '\0'; ++str1_ptr) {};

  for (char *str2_ptr = str2; *str2_ptr != '\0'; ++str2_ptr, ++str1_ptr)
  {
    *str1_ptr = *str2_ptr;
  }
  *str1_ptr = '\0';
}


int main(void) {
  uint8_t num = 234;
  char buffer[MAX_CHAR];

  str_write_num(buffer, num);
  printf("%s\n", buffer);

  char *unit = "m^2";
  str_cat(buffer, unit);
  printf("%s\n", buffer);

  return 0;
}
