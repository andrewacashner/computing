#define MAX_FILENAME 50 \

/*2:*/
#line 57 "./biblio.w"

#include <stdio.h> 
#include <stdlib.h> 

int main(int argc,char*argv[])
{
char infilename[MAX_FILENAME];
char outfilename[MAX_FILENAME];
FILE infile;
FILE outfile;
if(argc!=3){
fprintf(stderr,"Incorrect number of arguments.\n");
exit(EXIT_FAILURE);
}
printf("Read file %s and add to bib file %s.\n",argv[1],argv[2]);
return(0);
}


/*:2*/
