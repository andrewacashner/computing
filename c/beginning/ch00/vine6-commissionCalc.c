#include <stdio.h>

main()
{
	float fRate = 0;
	float fSalesPrice = 0;
	float fCost = 0;
	float fCommission = 0;

	//Commission = Rate * (SalesPrice - Cost)

	printf("\nCOMMISSION CALCULATOR, by Andrew Cashner\n");

	printf("\nEnter the employee's rate of commission (in percent): ");
	scanf("%f", &fRate);

	printf("Enter the price of the item sold: $");
	scanf("%f", &fSalesPrice);

	printf("Enter the cost of the item: $");
	scanf("%f", &fCost);

	fCommission = (fRate/100) * (fSalesPrice - fCost);

	printf("----------------------------\n");
	printf("Employee has earned $%.2f in commission.\n\n", 
		fCommission);
}



