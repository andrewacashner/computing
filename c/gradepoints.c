/* Calculate grade points out of a given total:
 * E.g., if test answer is worth 25 points, give
 * point values for A, B, C answer
 * -- 2016/10/24
 */

#include <stdio.h>
#include <stdlib.h>

const enum {
     GRADE_A, GRADE_A_MINUS,
     GRADE_B_PLUS, GRADE_B, GRADE_B_MINUS,
     GRADE_C_PLUS, GRADE_C, GRADE_C_MINUS,
     GRADE_D,
     GRADE_F,
     TOTAL_GRADE_DIVISIONS
} letter_grade_index;

const float letter_grade_min_percent[] = {
     0.93, 0.90,
     0.87, 0.83, 0.80,
     0.77, 0.73, 0.70,
     0.60,
     0.50
};

const char *letter_grade_name[] = {
     "A", "A-",
     "B+", "B", "B-",
     "C+", "C", "C-",
     "D",
     "F"
};

int main(int argc, char *argv[])
{
     float max_points;
     float letter_grade_value[TOTAL_GRADE_DIVISIONS];
     int i;

     if (argc != 2) {
	  fprintf(stderr, "Incorrect arguments. "
		  "Usage: gradepoints <no. of total points>\n");
	  exit(EXIT_FAILURE);
     }

     if (sscanf(argv[1], "%f", &max_points) != 1) {
	  fprintf(stderr, "Error: Bad argument %s. Should be number.\n", argv[1]);
	  exit(EXIT_FAILURE);
     }

     for (i = 0; i < TOTAL_GRADE_DIVISIONS; ++i) {
	  letter_grade_value[i] = max_points * letter_grade_min_percent[i];
	  printf("%6.2f  %s\n", letter_grade_value[i], letter_grade_name[i]);
     }
     
     return(0);
}
