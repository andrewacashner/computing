/* Data and pointers to data */

#include <stdio.h>

int main(void) {
	
	char *abc[] = { 	"aardvark", "bacon", "chestnut",
				"donut", "exercise", "froglet",
				"garnish", "herpetologist", "iguana",
				"jeremiad", "kilometer", "leap",
				"missile", "neuron", "on",
				"perpendicular", "quietus", "sarcastic",
				"tomato", "uvula", "vervet", "walrus",
				"yard", "zed" };
	char **abc_ptr;

	for (abc_ptr = abc; *(abc_ptr + 1) != NULL; abc_ptr++)
		printf("%s ", *abc_ptr);
	printf("\n");

	return(0);
}

	
