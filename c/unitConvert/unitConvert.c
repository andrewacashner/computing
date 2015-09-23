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
* 3. Volume (to build)
* 4. Mass/Weight (to build)
* 5. Temperature (to build)
*
* UNIT CODES
* Length: 0 mm, 1 cm, 3 m (must be entered as _m), 6 km (=powers of 10 * mm)
* Area: 0 mm2, 2 cm2, 6 m2, 10 ha (hectare = 10,000 m2), 12 km2 
*
* Length: 1 in, 12 ft, 36 yd, 63360 mi (=numbers of inches in each)
* Area: 1 in2, 144 ft2, 1296 yd2, 6272640 ac (acre), 4014489600 mi2
*
* CONVERSION TYPES (S Standard, M Metric)
* 1. M-M
* 2. S-S
* 3. M-S
* 4. S-M
*
****************************************
*
*  Begin by parsing string to get units. 
*  Read units from input strings like this: 
*  43 cm to in OR 2.35 m to km OR 3 in2 to m2.
*  Counts number of characters in each word and then uses those numbers to copy
*  the correct characters to startUnit and endUnit variables.
*  
*  Reads input string. Trims end-of-line and trailing spaces. Uses loop to count
*  backward from end of string until it reaches a space; that count is assigned
*  to the last word. Then the count continues until another space is reached,
*  and this count is assigned to previous word.
************************************************************/

#include<stdio.h>
#include<string.h>
#include<math.h>

char  line[100]; 		/* User input buffer */
double startValue;		/* Starting numeric value pulled from input string */
int   repCounter; 		/* Counts repetitions of loop */
int   charCounter;		/* Counts characters */
int   endUnitLength;	/* Length of end unit input string */
int   startUnitLength; 	/* Length of start unit input string */
char  endUnit[100];		/* End unit for conversion */
char  startUnit[100];	/* Start unit for conversion */
double endValue;			/* Value after conversion, computed */
long int   unitClass[2][3];  /* Codes for unit system, measurement and unit type:
							first dimension is startUnit, 2nd is end */ 
int   conversionType;	/* Code for 4 possible conversions as in table */
int   possibility;		/* True (0) or false (1) */

/*************************************************************/

int main ()
{
	printf("\nUNIT CONVERTER");
	printf("\nEnter desired conversion. Example:");
	printf("\n 2.34 ft to m");
	printf("\nType h (help) for list of possible units and abbreviations.");
	printf("\nType q to quit.\n");

/*****************************************/

	while (1) {

/* GET VALUES AND UNITS TO CONVERT */
	
		possibility = 0; /* Set, then reset on loop */

		printf("\nConvert: ");
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%lf", &startValue); /* Get numeric float value */

	/* Check for help or quit */
		if ((line[0] == 'q') || (line[0] == 'Q'))
			break;
		switch (line[0]) {
			case 'h':
			case 'H':	/* Help */
	printf("\n* HELP: ");
	printf("\n* Convert within or between standard and metric unit systems.");
	printf("\n* ");
	printf("\n* Follow these examples of syntax:");
	printf("\n*  1.2 in to cm");
	printf("\n*  1.2 inches to centimeters");
	printf("\n*  1.2 ha to m2");
	printf("\n*  1.2 hectares to square-meters");
	printf("\n* ");
	printf("\n* You can enter any of these units as given here:");
	printf("\n*  METRIC LENGTH:");
	printf("\n*   mm millimeter millimeters");
	printf("\n*   cm centimeter centimeters");
	printf("\n*   m  meter      meters");
	printf("\n*   km kilometer  kilometers");
	printf("\n*  METRIC AREA:");
	printf("\n*   mm2 square-millimeter square-millimeters");
	printf("\n*   cm2 square-centimeter square-centimeters");
	printf("\n*   m2  square-meter      square-meters");
	printf("\n*   ha  hectare           hectares");
	printf("\n*   km2 square-kilometer  square-kilometers");
	printf("\n*  STANDARD LENGTH:");
	printf("\n*   in inch inches");
	printf("\n*   ft foot feet");
	printf("\n*   yd yard yards");
	printf("\n*   mi mile miles");
	printf("\n*  STANDARD AREA:");
	printf("\n*   in2 square-inch square-inches");
	printf("\n*   ft2 square-foot square-feet");
	printf("\n*   yd2 square-yard square-yards");
	printf("\n*   ac  acre        acres");
	printf("\n*   mi2 square-mile square-miles");
	printf("\n* ");
	printf("\n* Type q to quit.\n");
				break;

	 		default:

		/* Trim trailing characters from string */

		/*Trim end-of-line character \n */
		line[strlen(line)-1] = '\0';

		/*Trim trailing spaces */
		while (1) {
			if (line[strlen(line)-1] == ' ') {
				line[strlen(line)-1] = '\0';
			} else break;
		}

		/* Count backward from end of string to get number
		*  of characters per word */

		charCounter = strlen(line) - 1;

		for (repCounter = 0; repCounter < (strlen(line)); ++repCounter) {
	
			/* Look for end unit */		
			if (line[charCounter] == ' ') {

				endUnitLength = repCounter; /* Get length of last word */
					/* Skip the space, then continue */
				charCounter = charCounter - 4;	
				/* Look for start unit (skip "_to_" in between) */
				for (repCounter = 0; repCounter < (strlen(line)); ++repCounter) {

					if (line[charCounter] == ' ') {

						startUnitLength = repCounter; /* Length of first word */
						break; 

					} else {
						--charCounter;
						continue;
					} 
				}
				break;

			} else --charCounter;
		}

		/* Copy unit values */
			/* End Unit */
	
		for (repCounter = 0; repCounter < (endUnitLength); ++repCounter) {

		endUnit[repCounter] = line[strlen(line) - endUnitLength + repCounter];
		}

		endUnit[endUnitLength] = '\0';

			/* Start Unit */
	
				/* Move to beginning of start unit */
		charCounter = endUnitLength + startUnitLength + 4;

		for (repCounter = 0; repCounter < (startUnitLength); ++repCounter) {

			startUnit[repCounter] = line[strlen(line) - charCounter];
			--charCounter;
		}	
		startUnit[startUnitLength] = '\0';

	/* Debug 
	printf("\nConvert %f %s to %s.", startValue, startUnit, endUnit);
	printf("\nIs string = ''mm''? %d", (strcmp(startUnit, "mm"))); */

/*****************************************
* STARTING UNIT: SET CODES for unitClass */

	/* Metric units: Length */

		if ((strcmp(startUnit, "mm") == 0) ||  
			(strcmp(startUnit, "millimeter") == 0) ||
			(strcmp(startUnit, "millimeters") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 0; /* Mm */
		} else if ((strcmp(startUnit, "cm") == 0) ||
			(strcmp(startUnit, "centimeter") == 0) ||
			(strcmp(startUnit, "centimeters") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 1; /* Centimeter */
		} else if ((strcmp(startUnit, "m") == 0) ||
			(strcmp(startUnit, "meter") == 0) ||
			(strcmp(startUnit, "meters") == 0)) {
				unitClass[0][0] = 1; /* Metric */	
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 3; /* Meter */
		} else if ((strcmp(startUnit, "km") == 0) ||
			(strcmp(startUnit, "kilometer") == 0) ||
			(strcmp(startUnit, "kilometers") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 6; /* Kilometer */
	
	/* Metric units: Area */

		} else if ((strcmp(startUnit, "mm2") == 0) ||
			(strcmp(startUnit, "square-millimeter") == 0) ||
			(strcmp(startUnit, "square-millimeters") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 0; /* mm2 */
		} else if ((strcmp(startUnit, "cm2") == 0) ||
			(strcmp(startUnit, "square-centimeter") == 0) ||
			(strcmp(startUnit, "square-centimeters") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 2; /* cm2 */
		} else if ((strcmp(startUnit, "m2") == 0) ||
			(strcmp(startUnit, "square-meter") == 0) ||
			(strcmp(startUnit, "square-meters") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 6; /* m2 */
		} else if ((strcmp(startUnit, "ha") == 0) ||
			(strcmp(startUnit, "hectare") == 0) ||
			(strcmp(startUnit, "hectares") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 10; /* ha */
		} else if ((strcmp(startUnit, "km2") == 0) ||
			(strcmp(startUnit, "square-kilometer") == 0) ||
			(strcmp(startUnit, "square-kilometers") == 0)) {
				unitClass[0][0] = 1; /* Metric */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 12; /* km2 */
		
	/* Standard units: Length */

		} else if ((strcmp(startUnit, "in") == 0) ||
			(strcmp(startUnit, "inch") == 0) ||
			(strcmp(startUnit, "inches") == 0)) {
				unitClass[0][0] = 2; /* Standard unit */
				unitClass[0][1] = 1; /* Length type */
				unitClass[0][2] = 1; /* Inch */
		} else if ((strcmp(startUnit, "ft") == 0) ||
			(strcmp(startUnit, "foot") == 0) ||
			(strcmp(startUnit, "feet") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 12; /* Foot */
		} else if ((strcmp(startUnit, "yd") == 0) ||
			(strcmp(startUnit, "yard") == 0) ||
			(strcmp(startUnit, "yards") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 36; /* Yard */
		} else if ((strcmp(startUnit, "mi") == 0) ||
			(strcmp(startUnit, "mile") == 0) ||
			(strcmp(startUnit, "miles") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 1; /* Length */
				unitClass[0][2] = 63360; /* Mile */

	/* Standard units: Area */

		} else if ((strcmp(startUnit, "in2") == 0) ||
			(strcmp(startUnit, "square-inch") == 0) ||
			(strcmp(startUnit, "square-inches") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 1; /* in2 */
		} else if ((strcmp(startUnit, "ft2") == 0) ||
			(strcmp(startUnit, "square-foot") == 0) ||
			(strcmp(startUnit, "square-feet") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 144; /* ft2 */
		} else if ((strcmp(startUnit, "yd2") == 0) ||
			(strcmp(startUnit, "square-yard") == 0) ||
			(strcmp(startUnit, "square-yards") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 1296; /* yd2 */
		} else if ((strcmp(startUnit, "ac") == 0) ||
			(strcmp(startUnit, "acre") == 0) ||
			(strcmp(startUnit, "acres") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 6272640; /* ac */
		} else if ((strcmp(startUnit, "mi2") == 0) ||
			(strcmp(startUnit, "square-mile") == 0) ||
			(strcmp(startUnit, "square-miles") == 0)) {
				unitClass[0][0] = 2; /* Standard */
				unitClass[0][1] = 2; /* Area */
				unitClass[0][2] = 4014489600; /* mi2 */


	/* Unrecognized units: Code to produce error message */

		} else {
			possibility = 1;
		}

/*****************************************
* ENDING UNIT: SET CODES for unitClass   */

	/* Metric units: Length */

		if ((strcmp(endUnit, "mm") == 0) ||
			(strcmp(endUnit, "millimeter") == 0) ||
			(strcmp(endUnit, "millimeters") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 0; /* Mm */
		} else if ((strcmp(endUnit, "cm") == 0) ||
			(strcmp(endUnit, "centimeter") == 0) ||
			(strcmp(endUnit, "centimeters") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 1; /* Centimeter */
		} else if ((strcmp(endUnit, "m") == 0) ||
			(strcmp(endUnit, "meter") == 0) ||
			(strcmp(endUnit, "meters") == 0)) {
				unitClass[1][0] = 1; /* Metric */	
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 3; /* Meter */
		} else if ((strcmp(endUnit, "km") == 0) ||
			(strcmp(endUnit, "kilometer") == 0) ||
			(strcmp(endUnit, "kilometers") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 6; /* Kilometer */

	/* Metric units: Area */

		} else if ((strcmp(endUnit, "mm2") == 0) ||
			(strcmp(endUnit, "square-millimeter") == 0) ||
			(strcmp(endUnit, "square-millimeters") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 0; /* mm2 */
		} else if ((strcmp(endUnit, "cm2") == 0) ||
			(strcmp(endUnit, "square-centimeter") == 0) ||
			(strcmp(endUnit, "square-centimeters") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 2; /* cm2 */
		} else if ((strcmp(endUnit, "m2") == 0) ||
			(strcmp(endUnit, "square-meter") == 0) ||
			(strcmp(endUnit, "square-meters") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 6; /* m2 */
		} else if ((strcmp(endUnit, "ha") == 0) ||
			(strcmp(endUnit, "hectare") == 0) ||
			(strcmp(endUnit, "hectares") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 10; /* ha */
		} else if ((strcmp(endUnit, "km2") == 0) ||
			(strcmp(endUnit, "square-kilometer") == 0) ||
			(strcmp(endUnit, "square-kilometers") == 0)) {
				unitClass[1][0] = 1; /* Metric */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 12; /* km2 */
	
	/* Standard units: Length */

		} else if ((strcmp(endUnit, "in") == 0) ||
			(strcmp(endUnit, "inch") == 0) ||
			(strcmp(endUnit, "inches") == 0)) {
				unitClass[1][0] = 2; /* Standard unit */
				unitClass[1][1] = 1; /* Length type */
				unitClass[1][2] = 1; /* Inch */
		} else if ((strcmp(endUnit, "ft") == 0) ||
			(strcmp(endUnit, "foot") == 0) ||
			(strcmp(endUnit, "feet") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 12; /* Foot */
		} else if ((strcmp(endUnit, "yd") == 0) ||
			(strcmp(endUnit, "yard") == 0) ||
			(strcmp(endUnit, "yards") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 36; /* Yard */
		} else if ((strcmp(endUnit, "mi") == 0) ||
			(strcmp(endUnit, "mile") == 0) ||
			(strcmp(endUnit, "miles") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 1; /* Length */
				unitClass[1][2] = 63360; /* Mile */

	/* Standard units: Area */

		} else if ((strcmp(endUnit, "in2") == 0) ||
			(strcmp(endUnit, "square-inch") == 0) ||
			(strcmp(endUnit, "square-inches") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 1; /* in2 */
		} else if ((strcmp(endUnit, "ft2") == 0) ||
			(strcmp(endUnit, "square-foot") == 0) ||
			(strcmp(endUnit, "square-feet") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 144; /* ft2 */
		} else if ((strcmp(endUnit, "yd2") == 0) ||
			(strcmp(endUnit, "square-yard") == 0) ||
			(strcmp(endUnit, "square-yards") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 1296; /* yd2 */
		} else if ((strcmp(endUnit, "ac") == 0) ||
			(strcmp(endUnit, "acre") == 0) ||
			(strcmp(endUnit, "acres") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 6272640; /* ac */
		} else if ((strcmp(endUnit, "mi2") == 0) ||
			(strcmp(endUnit, "square-mile") == 0) ||
			(strcmp(endUnit, "square-miles") == 0)) {
				unitClass[1][0] = 2; /* Standard */
				unitClass[1][1] = 2; /* Area */
				unitClass[1][2] = 4014489600; /* mi2 */

	/* Unrecognized units: Code to produce error message */

		} else {
			possibility = 1;
		}

/* Debug 

		printf("\nSTART UNIT: unit system %d, type %d, specific code %d", 
			unitClass[0][0], unitClass[0][1], unitClass[0][2]);
		printf("\nEND UNIT:   unit system %d, type %d, specific code %d\n",
			unitClass[1][0], unitClass[1][1], unitClass[1][2]);
		printf("\nPossibility = %d", possibility);
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
		/* Length */

		} else if ((conversionType == 3) && (unitClass[0][1] == 1)) {

			/* 25.4 mm = 1 in. 1 mm = (1 in) / 25.4 
			*  Multiply metric by factor of 10 based on 
			*  code of metric unit (mm = 1). 
			*  Then divide by standard-unit factor (relationship to 
			*  inches as expressed in specific unit code) */
				
			endValue = startValue / (25.4 * pow(10, (- unitClass[0][2]) )
											* unitClass[1][2]);
		/* Area */

		} else if ((conversionType == 3) && (unitClass[0][1] == 2)) {

			/* Same as length but conversion factor (25.4) is squared) */

			endValue = startValue / (645.16 * pow(10, (- unitClass[0][2]) )
											* unitClass[1][2]);


	/*********************************/	
	/* STANDARD to METRIC conversion */
		/*Length */

		} else if ((conversionType == 4) && (unitClass[0][1] == 1)) {
			
			/* Inverse of conversion type 3 for length */

			endValue = startValue * 25.4 * pow(10, (- unitClass[1][2]) ) 
						* unitClass[0][2];

		/* Area */

		} else if ((conversionType == 4) && (unitClass[0][1] == 2)) {
			
			/* Inverse of conversion type 3 for area */

			endValue = startValue * 645.16 * pow(10, (- unitClass[1][2]) ) 
						* unitClass[0][2];
		}

/****************************************/		

/* PRINT ERROR or RESULT */

		if (possibility == 1) {
			printf("\nCannot convert %s to %s.", startUnit, endUnit);
			printf("\nTry different units (h for help,q to quit).\n");
		} else {
			printf("\nRESULT: %f %s = %f %s\n", 
				startValue, startUnit, endValue, endUnit);
		}
		
	  }
	}

	printf("\n");
	return(0);
}



