/* practicing with struct to make quasi db 
 * AAC , 2014-10-20
 */

#include <stdio.h>
#include <string.h>

#define MAX_STR 100

typedef struct person {
	char name_last[MAX_STR];
	char name_first[MAX_STR];
	enum { female, male } sex;
	int birth_yr;
	float height_m;
	float weight_kg;
} proto;

struct person newPerson(char[], char[], char[], int, float, float);

void printData(struct person);

int main(void)
{
	struct person WallaceB, WallaceJ;
	WallaceB = newPerson("Wallace", "Bob", "male", 1972, 1.8, 200);
	WallaceJ = newPerson("Wallace", "Jill", "female", 1975, 1.5, 120.34);

	printData(WallaceB);
	printData(WallaceJ);

	return(0);
}

struct person newPerson(char name_last[], char name_first[],
			char sex[], int birth_yr, 
			float height_m, float weight_kg)
{
	struct person tmp;

	strcpy(tmp.name_last, name_last);
	strcpy(tmp.name_first, name_first);
	if (strcmp(sex, "female") != 0)
		tmp.sex = 0;
	else tmp.sex = 1;
	tmp.birth_yr = birth_yr;
	tmp.height_m = height_m;
	tmp.weight_kg = weight_kg;

	return(tmp);
}

void printData(struct person tmp)
{
	printf("\n%s %s\n", tmp.name_first, tmp.name_last);
	printf("%s, born %d, height %.2f m, weight %.2f kg\n", 
		tmp.sex ? "female": "Female" ? "male": "Male",
		tmp.birth_yr, tmp.height_m, tmp.weight_kg);
	return;
}

