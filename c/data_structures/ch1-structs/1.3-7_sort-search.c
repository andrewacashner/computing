/* SELECTION SORT and BINARY SEARCH
 * Horowitz et al, Fundamentals of Data Structures in C (1993)
 * Program 1.3, 1.7
 * AAC, 2014-10-15
 */

#include <stdlib.h> 			/* was missing */
#include <stdio.h>
#include <math.h>

#define MAX_SIZE 101
#define SWAP(x,y,t)  ((t) = (x), (x) = (y), (y) = (t))
#define COMPARE(x,y) (((x) < (y)) ? -1: ((x) == (y))? 0: 1)

void sort(int[], int); 			/* selection sort */
int binsearch(int[], int, int, int); 	/* binary sort */
int main(void)  			/* was void main(void) */
{
	int i,n,q;
	int list[MAX_SIZE];

	/* Get max numbers < MAX_SIZE */
	printf("Enter the number of numbers to generate: ");
	scanf("%d", &n);
	if (n < 1 || n > MAX_SIZE) {
		fprintf(stderr, "Improper value of n\n");
		exit(1);
	}
	
	/* Randomly generate numbers */
	for (i = 0; i < n; i++) {
		list[i] = rand() % 1000;
		printf("%d ", list[i]);
	}

	/* Sort array into increasing order */
	sort(list, n);
	
	/* Print out sorted numbers */
	printf("\n Sorted array:\n ");
	for (i = 0; i < n; i++)
		printf("%d ", list[i]);
	printf("\n");

	/* Get search queryfor array */
	printf("Enter a number to search for: ");
	scanf("%d", &q);
	if ((i = binsearch(list, q, 1, n-1)) != -1)
		printf("Search term %d found at array index [%d].\n", q, i);
	else printf("Search term %d not found in array.\n", q);
	
	return(0);
}

void sort(int list[], int n)
{
	int i, j, min, temp;
	for (i = 0; i < n - 1; i++) {
		min = i;
		for (j = i + 1; j < n; j++)
			if (list[j] < list[min])
				min = j;
		SWAP(list[i],list[min],temp);
	}
}

int binsearch(int list[], int searchnum, int left, int right)
{
	/* Search list[0] <= list[1] <= ... <= list[n-1] for searchnum.
	 * Return its position if found. Otherwise return -1.
	 * Iterative implementation.
	 */
	 int middle;
	 if (left <= right) {
	 	middle = (left + right) / 2;
		switch (COMPARE(list[middle], searchnum)) {
			case -1: return binsearch(list, searchnum, middle + 1, right);
			case  0: return middle;
			case  1: return binsearch(list, searchnum, left, middle - 1);
		}
	}
	return -1;
}


