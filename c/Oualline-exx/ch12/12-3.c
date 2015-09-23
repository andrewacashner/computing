/* Andrew Cashner 2013-10-12 Oualline exercise 12-3 and 12-4
*
*  12-3:
*  Design an airline reservation data structure that contains the following
*  data: flight number, originating airport code (three characters),
*  destination airport code (three characters), starting time, arrival time
*
*  12-4:
*  Write a program that lists all the planes that leave from two airports
*  specified by the user.
*/

# include <stdio.h>
# include <string.h>

# define MAX 5		/* Maximum entries in database */

/* Structure for airline schedule data */
struct schedule {
	int 	flight_number ;		/* Flight number */
	char 	origin [4] ;		/* Originating airport code (3 characters) */
	char 	destination [4] ; 	/* Destination airport code */
	int		time_departure ;	/* Four-digit departure time, 24-hr clock */
	int		time_arrival ;		/* Four-digit arrival time */
} ;

/* Flight database: maximum number from MAX */
struct schedule flights [MAX] = {
	{1, "MAD", "ORD", 8, 9},
	{2, "ORD", "MAD", 9, 10},
	{3, "ORD", "LAX", 10, 11},
	{4, "LAX", "MAD", 11, 12},
	{5, "LAX", "ORD", 12, 13}
} ;


/* Query the database for departures and print results for single airport */
void search_departures (char query [4] )
{
	int  counter ;		/* Index through flights array with loop counter */
	int  success = 0 ;  /* Counter for successful searches */ 

	printf ("\nFlights departing from %s:\n", query) ;

	for (counter = 0 ; counter < MAX ; ++ counter )
	{
		if ( strcmp (flights[counter].origin, query) == 0 ) 
		{
			++ success ; 	/* Add to count of successes */
			printf ("Flight number:  %d\n", flights[counter].flight_number) ;
			printf ("Destination:    %s\n", flights[counter].destination) ;
			printf ("Departure time: %d\n", flights[counter].time_departure) ;
			printf ("Arrival time:   %d\n\n", flights[counter].time_arrival) ;
		} 
	}
	if (success == 0) 	/* Search returned no results */
		printf ("There are no flights departing from %s.\n", query) ;

	return ;
}


/* Search database for flights from two departing airports */
int main ()
{
	char 	line [100] ;	/* User input buffer */
	char	airport1 [4] ;	/* First departing airport to search */
	char 	airport2 [4] ;	/* Second airport */

	printf ("AIRLINE SCHEDULE DATABASE\n") ;

/* Get search terms */
	printf ("\nSearch for departing flights from two airports.\n") ;

	printf ("Enter three-letter code for first airport: ") ;
	fgets  (line, sizeof (line), stdin) ;
	line [3] = '\0' ;
	strcpy (airport1, line) ;

	printf ("Enter code for second airport: ");
	fgets  (line, sizeof (line), stdin) ;
	line [3] = '\0' ;
	strcpy (airport2, line) ;

/* Query the database */
	search_departures (airport1) ;
	search_departures (airport2) ;

	return (0) ;
}
