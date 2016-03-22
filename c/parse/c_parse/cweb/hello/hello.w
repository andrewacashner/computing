This is \.{hello}, a program that prints a greeting message to standard output.
It is a first attempt at using \.{CWEB}, by Andrew Cashner, \today.

@ Include header files.

@c
#include <stdio.h>

@ Define the string to print.

@c
static const char greeting[] = "Hello, world!";

@ Print the message and return.

@c
int main(void)
{
	printf("%s\n", greeting);
	return(0);
}

