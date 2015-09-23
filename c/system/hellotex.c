/* hellotex.c -- Use system() to print message using TeX */

#include <stdio.h>
#include <stdlib.h>

void weather(void);
void quit(void);

int main(void)
{
	int n;
	printf("Hello Betty!\n");
	printf("Press a number and then return and I will do what you say.\n");
	printf("1 -- Print today's weather forecast\n");
	printf("2 -- End this program\n\n");
	printf("Which would you like me to do? ");
	scanf("%d", &n);
	switch (n) {
		case 1: 
			weather(); 
			break;
		case 2: 
			break;
		default: 
			printf("Please type one of the number options: "); 
	}
	quit();
	return(0);
}

void weather(void)
{
	system("pdflatex --interaction batchmode frame");
	system("lp frame.pdf");
	return;
}

void quit(void)
{
	printf("Have a good day. Good bye.\n");
	return;
}
