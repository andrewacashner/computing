/****************************************
* UNIT CONVERTER
* Andrew Cashner, 2012-03
*
* Convert most any measurement units to other measurement units.
*
****************************************/

/****************************************
*
* UNIT SYSTEMS
* 1. Metric
* 2. Standard
*
* MEASUREMENT TYPES
* 1. Length 
* 2. Area
*
* UNIT CODES
* 0 mm, 1 cm, 3 m (must be entered as _m), 6 km (=powers of 10 * mm)
* 0 mm^2, 1 cm^2, 3 m^2, 6 km^2, 7 ha (hectare = 10,000 m^2)
*
* 1 in, 12 ft, 36 yd, 63360 mi (=numbers of inches in each)
* 1 in^2, 12 ft^2, 36 yd^2, 99 ac (acre = 1/640 mi^2), 63360 mi^2
*
* CONVERSION TYPES (S Standard, M Metric)
* 1. M-M
* 2. S-S
* 3. M-S
* 4. S-M
*
****************************************/

#include<stdio.h>
#include<string.h>
#include<math.h>

char  line[100];		/* User input buffer */
int   repCounter;		/* Counter used for repetitions of loop */
int   charCounter;		/* Counter used to count characters in string */
char  startUnit[100];   /* Starting unit, input by user */
char  endUnit[100];		/* Unit to be converted into, input by user */
float startValue;		/* Starting value, input by user */
float endValue;			/* Value after conversion, computed */
int   unitClass[2][3];  /* Codes for unit system, measurement and unit type:
							first dimension is startUnit, 2nd is end */ 
int   conversionType;	/* Code for 4 possible conversions as in table */
int   possibility;		/* True (0) or false (1) */

/****************************************/

int main()
{

	printf("\nUNIT CONVERTER");
	printf("\nEnter desired conversion using 2-letter abbreviations.");
	printf("\nExample: 2.34 cm to in (Use _m for meters).");
	printf("\nType h (help) for list of possible units or q to quit.\n");

/*****************************************/

	while (1) {

/* GET VALUES AND UNITS TO CONVERT */
	
		possibility = 0; /* Set or reset */

		printf("\nConvert: ");
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%f", &startValue); /* Get numeric float value */

	/* Check for help or quit */
		if (line[0] == 'q') {
			break;
		} else if (line[0] == 'h') {

		printf("\n* HELP: ");
		printf("\n( Prototype can convert any pair of measurements.");
		printf("\n* Metric or standard.");
		printf("\n* Use these abrreviations for the following units:");
		printf("\n* METRIC:");
		printf("\n* mm (millimeter), mm (centimeter), m (meter),");
		printf("\n* km (kilometer), mm2 (square mm), cm2, m2, km2,");
		printf("\n* ha (hectare)"); 
		printf("\n* STANDARD:");
		printf("\n* in (inch), ft (foot), yd (yard), mi (mile),");
		printf("\n* in2 (square in), ft2, yd2, mi2, ac (acre)"); 
		printf("\n* Type q to quit.\n");
		continue;

		} else {

		/* Pull unit characters from string */

		for (counter = 1, counter < 100, ++counter) {
			
			/*Find no. of characters after last space*/
			if (line[strlen(line)-(counter+1)] == ' ') {
				endUnitLength = counter;
			    ++counter; /* skip the space */	
				
				/*Find no. of characters after first space */
				if (line[strlen(line)-(counter+1)] == ' ') {
					startUnitLength = counter;
					break;
				}
			}

		}

	
	/* Debug */

	printf("startUnitLength %d, endUnitLength %d", startUnitLength,
	endUnitLength);

	
/*
strlen(line)-
       98765432 1 
     5 km to mi\n\0
      3 km to m\n\0
   23 m2 to in2\n\0
23.1 km2 to mi2\n\0
*/

	/* Get end unit */
		
		for (counter = 0, counter < (endUnitLength), ++counter) {

			endUnit[counter] = line[strlen(line)-(endUnitLength + 1)];
		}
		
		endUnit[endUnitLength-1] = '\0';

	/* Get start unit */

		for (counter = 0, counter < (startUnitLength), ++counter) {

			startUnit[counter] =
			line[strlen(line)-(endUnitLength+startUnitLength+7)];
		}

		startUnit[startUnitLength-1] = '\0';

	/* Debug */

	printf("startUnit %s, endUnit %s", startUnit, endUnit);

	
	/* Get start unit by pulling characters from input string 
		startUnit[0] = line[strlen(line)-9];
		startUnit[1] = line[strlen(line)-8];
		startUnit[2] = '\0';
		*/
		
/* Debug *

		printf("\nConvert %.4f %s to %s.", startValue, startUnit, endUnit);
*/
/*****************************************
* STARTING UNIT: SET CODES for unitClass */

	/* Metric units: Length */

		if (strcmp(startUnit, "mm") == 0) {
			unitClass[0][0] = 1; /* Metric */
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 0; /* Mm */
		} else if (strcmp(startUnit, "cm") == 0) {
			unitClass[0][0] = 1; /* Metric */
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 1; /* Centimeter */
		} else if (strcmp(startUnit, "_m") == 0) {
			unitClass[0][0] = 1; /* Metric */	
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 3; /* Meter */
		} else if (strcmp(startUnit, "km") == 0) {
			unitClass[0][0] = 1; /* Metric */
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 6; /* Kilometer */

	/* Standard units: Length */

		} else if (strcmp(startUnit, "in") == 0) {
			unitClass[0][0] = 2; /* Standard unit */
			unitClass[0][1] = 1; /* Length type */
			unitClass[0][2] = 1; /* Inch */
		} else if (strcmp(startUnit, "ft") == 0) {
			unitClass[0][0] = 2; /* Standard */
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 12; /* Foot */
		} else if (strcmp(startUnit, "yd") == 0) {
			unitClass[0][0] = 2; /* Standard */
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 36; /* Yard */
		} else if (strcmp(startUnit, "mi") == 0) {
			unitClass[0][0] = 2; /* Standard */
			unitClass[0][1] = 1; /* Length */
			unitClass[0][2] = 63360; /* Mile */

	/* Unrecognized units: Code to produce error message */

		} else {
			possibility = 1;
		}

/*****************************************
* ENDING UNIT: SET CODES for unitClass   */

	/* Metric units: Length */

		if (strcmp(endUnit, "mm") == 0) {
			unitClass[1][0] = 1; /* Metric */
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 0; /* Mm */
		} else if (strcmp(endUnit, "cm") == 0) {
			unitClass[1][0] = 1; /* Metric */
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 1; /* Centimeter */
		} else if (strcmp(endUnit, "_m") == 0) {
			unitClass[1][0] = 1; /* Metric */	
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 3; /* Meter */
		} else if (strcmp(endUnit, "km") == 0) {
			unitClass[1][0] = 1; /* Metric */
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 6; /* Kilometer */
		
	/* Standard units: Length */

		} else if (strcmp(endUnit, "in") == 0) {
			unitClass[1][0] = 2; /* Standard unit */
			unitClass[1][1] = 1; /* Length type */
			unitClass[1][2] = 1; /* Inch */
		} else if (strcmp(endUnit, "ft") == 0) {
			unitClass[1][0] = 2; /* Standard */
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 12; /* Foot */
		} else if (strcmp(endUnit, "yd") == 0) {
			unitClass[1][0] = 2; /* Standard */
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 36; /* Yard */
		} else if (strcmp(endUnit, "mi") == 0) {
			unitClass[1][0] = 2; /* Standard */
			unitClass[1][1] = 1; /* Length */
			unitClass[1][2] = 63360; /* Mile */

	/* Unrecognized units: Code to produce error message */

		} else {
			possibility = 1;
		}

/* Debug *

		printf("\nSTART UNIT: unit system %d, type %d, specific code %d", 
			unitClass[0][0], unitClass[0][1], unitClass[0][2]);
		printf("\nEND UNIT:   unit system %d, type %d, specific code %d\n",
			unitClass[1][0], unitClass[1][1], unitClass[1][2]);
*/

/***********************************
* ELIMINATE IMPOSSIBLE CONVERSIONS */

	/* Ensure both units are same type (length, temperature, etc.) */

		if (unitClass[0][1] != unitClass[1][1]) {
			possibility = 1;
		}
	
	/* Ensure specific units are not same (e.g., from in to in) 
	 * If both in same unit system and both have same specific code */

		if ( (unitClass[0][0] == unitClass[1][0]) && 
				(unitClass[0][2] == unitClass[1][2]) ) {
			possibility = 1;
		}

/***************************************
SET CONVERSION TYPE (metric, standard) */

		if (unitClass[0][0] == unitClass[1][0]) {
			if (unitClass[0][0] == 1) {
				conversionType = 1; 	/* Metric to metric */
			} else conversionType = 2;  /* Standard to standard */
		} 
		if (unitClass[0][0] < unitClass[1][0]) {
			conversionType = 3;			/* Metric to standard */
		} else if (unitClass[0][0] > unitClass[1][0] ) {
			conversionType = 4;			/* Standard to metric */
		}

/* Debug *

		printf("\nConversion type %d\n", conversionType);
*/	

/**************************************************************
* CONVERT VALUES for each conversionType and measurement type */

	/* METRIC to METRIC conversion */
		if (conversionType == 1) { 

			/* Multiply by 10 to nth power where n = difference between
			 * start and end specific unit codes */

			endValue = startValue * 
						pow(10, (unitClass[0][2] - unitClass[1][2]) );

	/***********************************/
	/* STANDARD to STANDARD conversion */

		} else if (conversionType == 2) {
			
			/* Divide start specific code by end specific code 
			*  because codes represent values in inches, so same denominator */

			endValue = ( startValue * unitClass[0][2] ) / unitClass[1][2] ;
		
	/*********************************/
	/* METRIC to STANDARD conversion */
		} else if (conversionType == 3) {

			/* 25.4 mm = 1 in. 1 mm = (1 in) / 25.4 
			*  Multiply metric by factor of 10 based on 
			*  code of metric unit (mm = 1). 
			*  Then divide by standard-unit factor (relationship to 
			*  inches as expressed in specific unit code) */
				
			endValue = startValue / (25.4 * pow(10, (- unitClass[0][2]) )
											* unitClass[1][2]);

	/*********************************/	
	/* STANDARD to METRIC conversion */
		} else if (conversionType == 4) {
			
			/* Inverse of conversion type 3 */

			endValue = startValue * 25.4 * pow(10, (- unitClass[1][2]) ) 
						* unitClass[0][2];
		}

/****************************************/		

/* PRINT ERROR or RESULT */

		if (possibility == 1) {
			printf("\nCannot convert. Try different units (h for help, q to quit).\n"); 
		} else {
			printf("\nRESULT: %f %s = %f %s\n", 
				startValue, startUnit, endValue, endUnit);
		}
		
		}
	}
	printf("\n");
	return(0);
}



