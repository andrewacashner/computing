int rand_lim(int limit) {
	/* return a random number between 0 and limit inclusive */

	int divisor = RAND_MAX/(limit+1);
	int return_value;

	do {
		return_value = rand() / divisor;
	} 	while (return_value > limit); 

	return (return_value);
}
