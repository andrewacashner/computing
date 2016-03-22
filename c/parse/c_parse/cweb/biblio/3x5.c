#define MAX_FILENAME 50
#define BUFFER_SIZE 1056
#define MAX_LINE 320 \

/*2:*/
#line 59 "./3x5.w"

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 

int main(int argc,char*argv[])
{
/*3:*/
#line 76 "./3x5.w"

char infilename[MAX_FILENAME];
char outfilename[MAX_FILENAME];
FILE*infile;
FILE*outfile;

/*:3*//*6:*/
#line 109 "./3x5.w"

char bibentry_buffer[BUFFER_SIZE];
char line[MAX_LINE];
int lines_read;
typedef enum{FALSE,TRUE}boolean;
boolean entry_end;

/*:6*/
#line 66 "./3x5.w"

/*4:*/
#line 84 "./3x5.w"

if(argc!=3){
fprintf(stderr,"Incorrect number of arguments.\n");
exit(EXIT_FAILURE);
}
printf("Read file %s and add to bib file %s.\n",argv[1],argv[2]);

/*:4*/
#line 67 "./3x5.w"

/*5:*/
#line 93 "./3x5.w"

strcpy(infilename,argv[1]);
strcpy(outfilename,argv[2]);
infile= fopen(infilename,"r");
if(infile==NULL){
fprintf(stderr,"Could not open file %s for reading.\n",infilename);
exit(EXIT_FAILURE);
}
outfile= fopen(outfilename,"wr");
if(outfile==NULL){
fprintf(stderr,"Could not open file %s for writing.\n",outfilename);
exit(EXIT_FAILURE);
}

/*:5*/
#line 68 "./3x5.w"

/*7:*/
#line 118 "./3x5.w"

bibentry_buffer[0]= '\0';
while(entry_end==FALSE){
fgets(line,sizeof(line),infile);
if(line==NULL||line[0]=='\n'){
entry_end= TRUE;
}
else{
strcat(bibentry_buffer,line);
++lines_read;
}
}
printf("%d lines read\n",lines_read);
printf("%s\n",bibentry_buffer);

fputs(bibentry_buffer,outfile);


/*:7*/
#line 69 "./3x5.w"

/*8:*/
#line 138 "./3x5.w"

fclose(infile);
fclose(outfile);



/*:8*/
#line 70 "./3x5.w"

return(0);
}

/*:2*/
