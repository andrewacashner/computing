/* Data structure for planets, Horowitz ch2 
 * AAC, 2014-10-29
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Structures */

enum planet_name {	
	MERCURY, VENUS, EARTH,
	MARS, JUPITER, SATURN,
	URANUS, NEPTUNE 
} planet_name;

typedef struct planet *planet_ptr;

typedef struct planet {
	enum planet_name name;
	float distance_mi; 	/* from sun */
	int moons; 		/* quantity of moons */
	planet_ptr next;
} planet;

/* Function prototypes */
planet_ptr create_planet(enum planet_name name, float, int, planet_ptr);

void print_planet(planet_ptr);

/* MAIN */
int main(void)
{
	planet_ptr 	solar_system, mercury, venus, earth, mars, 
			jupiter, saturn, uranus, neptune;
	
	neptune	= create_planet(NEPTUNE, 2798842000, 13, NULL);
	uranus	= create_planet(URANUS, 1783950000, 27, neptune);
	saturn	= create_planet(SATURN, 888188000, 47, uranus);
	jupiter	= create_planet(JUPITER, 483632000, 63, saturn);
	mars	= create_planet(MARS, 141635300, 2, jupiter);
	earth 	= create_planet(EARTH, 92957100, 1, mars);
	venus 	= create_planet(VENUS, 67232360, 0, earth);
	mercury	= create_planet(MERCURY, 35983610, 0, venus);
	solar_system = mercury;

	print_planet(solar_system);
	return(0);
}

planet_ptr create_planet(enum planet_name name, float distance_mi, int moons, planet_ptr next)
{
	planet_ptr tmp = malloc(sizeof(planet));
	tmp->name = name;
	tmp->distance_mi = distance_mi;
	tmp->moons = moons;
	tmp->next = next;
	return(tmp);
}

void print_planet(planet_ptr tmp)
{
	char *name_str[8] = {
		"Mercury", "Venus", "Earth", "Mars",
		"Jupiter", "Saturn", "Uranus", "Neptune"
	};
	if (tmp) {
		printf("The planet %s is %.0f miles from the sun "
			"and has %d %s.\n", 
			name_str[tmp->name], tmp->distance_mi, tmp->moons,
			((tmp->moons == 1) ? "moon" : "moons")
		);
		print_planet(tmp->next);
	}
	return;
}


