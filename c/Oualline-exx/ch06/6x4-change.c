/* Oualline, ch. 6, exercise 4: Make change
*  Andrew Cashner, 2012-02-23
*
*  Given an mount of money less than $1.00, computes the number of
*  quarters, dimes, nickels, and pennies needed.
* 
*  Program asks for amount of money, converts character-string input into
*  floating-point value and stores in float variable.
* Multiplies value by 100 to get amount in change.
* Divide that amount by 25 and store result in integer to get number of whole
* quarters (decimals not needed).
* Takes the remainder (the amount not payable in quarters) and then divides that
* by 10 to get the number of dimes. 
* Does same with remainder to get nickels and then pennies.
* One loop: divide by change amount, get remainder, move on to next change
* amount.
* Change amount determined by a counter variable: 1 = quarter, 2 = dime, 3 =
* nickel, 4 = penny.
* Loop ends when there is no remainder.
********************/

#include<stdio.h>

char inputMoney[5];	/* User input buffer */
float startAmount;	/* Decimal value captured from input */
int moneyToCount;	/* Value entered by user (nos. after decimal) */
int coinType;		/* Code for type of coin, e.g. 1=quarter */
int coinValue;		/* Number value of each coin used in division */
int coinNumber;		/* Whole number of coins calculated in loop */
int coinRemainder;	/* Remainder of currency left at end of each loop */
int quarters;		/* Number of quarters needed (computed) */
int dimes;			/* Number of dimes needed (computed) */
int nickels;		/* Number of nickels needed (computed) */
int pennies;		/* Number of pennies needed (computed) */

/********************/

int main()
{
	printf("\n==CHANGE MAKER==\n");
	printf("Calculate coins needed to make up a currency amount.\n\n");

	printf("Enter an amount of money (as a decimal) less than $1.00: $");
	fgets(inputMoney, sizeof(inputMoney), stdin);
	sscanf(inputMoney, "%f", &startAmount);

	moneyToCount = 100 * startAmount;

	coinType = 1; 		/* Start counter for coinType */

	while (coinType < 5)
	{
		if (coinType == 1)
		{
			coinValue = 25;
		}

		if (coinType == 2)
		{
			coinValue = 10;
		}

		if (coinType == 3)
		{
			coinValue = 5;
		}

		if (coinType == 4)
		{
			coinValue = 1;
		}
		
		coinNumber = moneyToCount / coinValue;
		coinRemainder = moneyToCount % coinValue;

		if (coinNumber > 0)
		{
			if (coinType == 1)
			{
				quarters = coinNumber;
			}

			if (coinType == 2)
			{
				dimes = coinNumber;
			}

			if (coinType == 3)
			{
				nickels = coinNumber;
			}

			if (coinType == 4)
			{
				pennies = coinNumber;
			}
				
			moneyToCount = coinRemainder;
			++coinType;
			continue;
		}

		if (coinNumber < 1) 
		{
			++coinType;
			continue;
		}
	}

	printf("\n=====RESULT=====\n");
	printf("Change for $%.2f:\n", startAmount);
	printf("================");

	if (quarters > 0)
	{
		printf("\n%d quarter", quarters);
		if (quarters > 1)
		{
			printf("s");
		}
	}

	if (dimes > 0)
	{
		printf("\n%d dime", dimes);
		
		if (dimes > 1)
		{
			printf("s");
		}
	}

	if (nickels > 0)
	{
		printf("\n%d nickel", nickels);
	
		if (nickels > 1)
		{
			printf("s");
		}
	}

	if (pennies > 0)
	{
		printf("\n%d penn", pennies);

		if (pennies == 1)
		{	
			printf("y");
		}

		if (pennies > 1)
		{
			printf("ies");
		}
	}

	printf("\n\n");

	return(0);
}
