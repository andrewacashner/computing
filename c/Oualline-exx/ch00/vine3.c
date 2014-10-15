#include <stdio.h>

main()
{
	//variable declarations
	int x;
	float y;
	char c;

	//variable initializations
	x = -.4443;
	y = 554.21425343;
	c = 'M';

	//printing variable contents to standard output
	printf("\nThe value of integer variable x is %d", x);
	printf("\nThe value of float variable y is %.3f", y);
	printf("\nThe value of character variable c is %c\n", c);

	//character data types
	char firstInitial = 'A';
	char secondInitial = 'A';
	char thirdInitial = 'C';

	//print characters
	printf("\nMy initials are %c. %c. %c.\n\n", 
	firstInitial, secondInitial, thirdInitial);

	//constants
	const int a = 15;
	const float PI = 3.1415926;
	
	printf("\nConstant values are %d and %.4f\n\n", a, PI);

}
