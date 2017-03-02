/* 12-2.c Andrew Cashner 2013-10-06 Oualline exercise 12-2
*  Design a structure to store time and date. Write a function to find the
*  difference between two times in minutes.
*/

# include <stdio.h>

struct time_date
{
	int year ;		/* 4 digits */
	int month ;		/* 2 digits */
	int day ;			/* 2 digits */
	int hour ;		/* 2 digits: 24-hour clock, military time */
	int minute ;		/* 2 digits */
} ; 

struct time_date xmas_morn = {
	2013, 12, 25, 06, 00 };

struct time_date xmas_dinner = {
	2013, 12, 25, 14, 00 };

/***************************************************/
int minute_difference (int hour1, int minute1, int hour2, int minute2)
{
	int time1 ;	/* In minutes only */
	int time2 ;	
	int difference ;	/* Difference between larger and smaller in minutes */

	time1 = minute1 + (hour1 * 60) ;	/* Convert time to minutes */
	time2 = minute2 + (hour2 * 60) ;

	if (time1 > time2)
		difference = time1 - time2 ;
	else if (time1 < time2)
		difference = time2 - time1 ;
	
	return (difference) ;

}

/***************************************************/
int main ()
{
	int minutes ;	/* Difference in minutes */

	printf ("Christmas morning will begin\n") ;
	printf ("on %d-%d-%d at %d:%d hours.\n\n", 
			xmas_morn.year, 
			xmas_morn.month,
			xmas_morn.day,
			xmas_morn.hour,
			xmas_morn.minute ) ;

	printf ("Christmas dinner will begin\n") ;
	printf ("on %d-%d-%d at %d:%d hours.\n\n", 
			xmas_dinner.year, 
			xmas_dinner.month,
			xmas_dinner.day,
			xmas_dinner.hour,
			xmas_dinner.minute ) ;

	minutes = minute_difference (xmas_morn.hour, xmas_morn.minute,
				xmas_dinner.hour, xmas_dinner.minute) ;

	printf ("Between Christmas morning and dinner there are %d minutes.\n",
			minutes) ;
		
	
	return (0) ;
}
