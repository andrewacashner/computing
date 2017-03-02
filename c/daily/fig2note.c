/* fig2note -- Convert numbers to music notes with reference to bass note *
 * Andrew Cashner, 2015-02-24
 */

/* DIATONIC MUSIC ADDITION 
 *
 * Given a starting note with its octave and an interval, calculate the
 * ending note that is the given interval above the starting note.
 * Convert note to number, 0--6.
 * Because we are using octave numbers based on C octaves,
 * C is first in scale.
 * Divide the interval number by 7: quotient is number of octaves and
 * remainder is notes above the original note within the octave.
 * Add the quotient to the octave number, add the remainder to the note
 * number.
 */

#include <stdio.h>

const char diatonic_scale[7] = {'C', 'D', 'E', 'F', 'G', 'A', 'B'};

int main(void)
{
	int start_note;
	int start_octave;
	int end_note;
	int end_octave;
	int octaves_above;
	int notes_above;
	int interval_above;

	start_note = 6; 
	start_octave = 3; 				
	interval_above = 15;				

	--interval_above; /* Adjust interval because we are adding to zero not one */
	octaves_above = interval_above / 7; 		
	end_octave = start_octave + octaves_above; 	
	
	notes_above = interval_above % 7; 		
	end_note = start_note + notes_above;		
	if (end_note > 6) {
		end_note = end_note - 7;
		end_octave++;
	}						
							 

	printf("Start %c%d plus interval of %d = %c%d\n", 
		diatonic_scale[start_note], start_octave, interval_above + 1,
		diatonic_scale[end_note], end_octave);
	
	return(0);
}
