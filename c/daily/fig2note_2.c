/* fig2note_2 
 * Convert numbers to music notes with reference to bass note 
 * Andrew Cashner, 2015-02-24
 *
 * v2 -- Converts pitch-class + octave notation to internal decimal
 *       note number
 *
 * DIATONIC MUSIC ADDITION 
 *
 * Given a starting note with its octave and an interval, calculate the
 * ending note that is the given interval above the starting note.
 * Treat octave and note as two digits in base 7, convert to decimal.
 * Because we are using octave numbers based on C octaves,
 * C is first in scale.
 * Divide the decimal note number number by 7: 
 * quotient is the octave and remainder is the pitch class number.
 */

#include <stdio.h>

const char diatonic_scale[7] = {'C', 'D', 'E', 'F', 'G', 'A', 'B'};

int main(void)
{
	int start_note; 
	int start_octave;
	int interval_above; /* 1 larger than number to add */
	int start_pitch_num;
	int end_pitch_num;
	int end_octave;
	int end_note;

	start_note = 6; 
	start_octave = 3; 				
	interval_above = 10;				

	start_pitch_num = start_octave * 7 + start_note;
	end_pitch_num = start_pitch_num + interval_above - 1;
	end_octave = end_pitch_num / 7;
	end_note = end_pitch_num % 7;

	printf("Start %c%d plus interval of %d = %c%d\n", 
		diatonic_scale[start_note], start_octave, interval_above,
		diatonic_scale[end_note], end_octave);
	
	return(0);
}
