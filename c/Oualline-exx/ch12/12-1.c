/* 12-1.c Andrew Cashner 2013-10-06
*  Oualline exercise 12-1
*  Design a structure to hold the data for a mailing list. Write a function to
*  print out the data. */

# include <stdio.h>

# define MAX 3

struct mailing
{
	char name [60] ;		/* Last name, first name */
	char address1 [60] ;	/* Two lines of street address */
	char address2 [60] ;	
	char city [40] ;	
	char state [2] ;		/* Two-character abbreviations */
	long int zip ;			/* Numeric zip code */
} ;

/* My mailing list */
struct mailing list [MAX] = {

	{
	"Cashner, Andrew", 
	"1234 S. 1st",
	"Apt. 1",
	"Snort",
	"ND",
	12345
	},

	{
	"Johnson, Dick",
	"4125 Big Ave.",
	"",
	"Longsnake",
	"OH",
	71712
	},

	{
	"Fella, Rowdy",
	"Sales Dept., Big Fella's Shoes",
	"613 N. Park Ave. Place Pnt.",
	"Pothole",
	"TN",
	99132
	}
} ;

/*******************************************************/

int main ()
{
	int count ;	/* Loop counter */

	for (count = 0; count < MAX; ++count)
	{
		printf ("%s\n%s\n%s\n%s\n%s\n%ld\n\n", 
				list[count].name, 
				list[count].address1, 
				list[count].address2, 
				list[count].city,
				list[count].state, 
				list[count].zip ) ;
	}

	return (0) ;
}
