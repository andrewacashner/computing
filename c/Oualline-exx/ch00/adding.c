#include <stdio.h>

int main()
{
	float a, b, c, d;
	printf("Enter the first value: ");
	scanf("%f", &a);
	printf("Enter the second value: ");
	scanf("%f", &b);
	printf("Enter the third value: ");
	scanf("%f", &c);
	d = a + b + c;
	printf("Result: %f + %f + %f = %f\n", a, b, c, d);
	return 0;
}
