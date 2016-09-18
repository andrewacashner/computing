#define CSNAME_LENGTH 20
#define MAX_CSNAMES 11
#define FILENAME_LENGTH 100 \

/*1:*/
#line 62 "./latex2html.w"

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 

/*3:*/
#line 86 "./latex2html.w"

enum{/*4:*/
#line 95 "./latex2html.w"

EMPH,TEXTBF,ENQUOTE,SECTION,SUBSECTION,SUBSUBSECTION,PARAGRAPH,ITEMIZE,
ENUMERATE,ITEM,PARA

/*:4*/
#line 87 "./latex2html.w"
}csname_label;
typedef struct{
char latex_name[CSNAME_LENGTH];
char html_name[CSNAME_LENGTH];
}csname_dict_entry;
csname_dict_entry csname_dict[MAX_CSNAMES];

/*:3*//*10:*/
#line 181 "./latex2html.w"

enum{TEXT,CSNAME,BEGIN,END,COMMENT}mode;

/*:10*/
#line 67 "./latex2html.w"

/*5:*/
#line 101 "./latex2html.w"

void new_csname(int label,char tex[],char html[]);

/*:5*//*20:*/
#line 313 "./latex2html.w"

void csname2html(char currentcsname[]);

/*:20*/
#line 68 "./latex2html.w"


int main(int argc,char*argv[])
{
/*8:*/
#line 140 "./latex2html.w"

int group;
int c;
char csnames[MAX_CSNAMES][CSNAME_LENGTH];

FILE*infile;
FILE*outfile;
char infilename[FILENAME_LENGTH];
char outfilename[FILENAME_LENGTH];
int char_position;

/*:8*/
#line 72 "./latex2html.w"

/*7:*/
#line 117 "./latex2html.w"

new_csname(EMPH,"emph","em");
new_csname(TEXTBF,"textbf","strong");
new_csname(ENQUOTE,"enquote","q");
new_csname(SECTION,"section","h1");
new_csname(SUBSECTION,"subsection","h2");
new_csname(SUBSUBSECTION,"subsubsection","h3");
new_csname(PARAGRAPH,"paragraph","h4");
new_csname(ITEMIZE,"itemize","ul");
new_csname(ENUMERATE,"enumerate","ol");
new_csname(ITEM,"item","li");
new_csname(PARA,"para","p");


/*:7*/
#line 73 "./latex2html.w"

/*9:*/
#line 153 "./latex2html.w"

if(argc!=2){
fprintf(stderr,"Usage: latex2html <filename>\n");
exit(EXIT_FAILURE);
}
strcpy(infilename,argv[1]);
if(strcmp(&infilename[strlen(infilename)-4],".tex")!=0){
strcat(infilename,".tex");
}
infile= fopen(infilename,"r");
if(infile==NULL){
fprintf(stderr,"Unable to open file %s for reading.\n",infilename);
exit(EXIT_FAILURE);
}
strcpy(outfilename,argv[1]);
if(strcmp(&outfilename[strlen(outfilename)-4],".tex")==0){
outfilename[strlen(outfilename)-4]= '\0';
}
strcat(outfilename,".html");
outfile= fopen(outfilename,"w");
if(outfile==NULL){
fprintf(stderr,"Unable to open file %s for writing.\n",outfilename);
exit(EXIT_FAILURE);
}


/*:9*/
#line 74 "./latex2html.w"

/*11:*/
#line 191 "./latex2html.w"

mode= TEXT;
group= 0;
char_position= 0;

while((c= fgetc(infile))!=EOF){
switch(mode){
/*12:*/
#line 210 "./latex2html.w"

case TEXT:
if(c=='\\'){
mode= CSNAME;
char_position= 0;
++group;
continue;
}
if(c=='}'){
mode= TEXT;
/*19:*/
#line 306 "./latex2html.w"

csname2html(csnames[group]);
fprintf(outfile,"</%s>",csnames[group]);


/*:19*/
#line 220 "./latex2html.w"

--group;
continue;
}
if(c=='%'){
mode= COMMENT;
continue;
}
fputc(c,outfile);
break;

/*:12*/
#line 198 "./latex2html.w"

/*13:*/
#line 234 "./latex2html.w"

case CSNAME:
if(c=='{'){
csnames[group][char_position]= '\0';
/*14:*/
#line 250 "./latex2html.w"

if(strcmp(csnames[group],"begin")==0){
char_position= 0;
mode= BEGIN;
continue;
}
if(strcmp(csnames[group],"end")==0){
mode= END;
continue;
}
/*:14*/
#line 238 "./latex2html.w"

/*18:*/
#line 299 "./latex2html.w"

csname2html(csnames[group]);
fprintf(outfile,"<%s>",csnames[group]);

/*:18*/
#line 239 "./latex2html.w"

mode= TEXT;
continue;
}
csnames[group][char_position]= (char)c;
++char_position;
break;

/*:13*/
#line 199 "./latex2html.w"

/*15:*/
#line 263 "./latex2html.w"

case BEGIN:
if(c=='}'){
csnames[group][char_position]= '\0';
/*18:*/
#line 299 "./latex2html.w"

csname2html(csnames[group]);
fprintf(outfile,"<%s>",csnames[group]);

/*:18*/
#line 267 "./latex2html.w"

mode= TEXT;
continue;
}
csnames[group][char_position]= (char)c;
++char_position;
break;

/*:15*/
#line 200 "./latex2html.w"

/*16:*/
#line 279 "./latex2html.w"

case END:
if(c=='}'){
mode= TEXT;
--group;
/*19:*/
#line 306 "./latex2html.w"

csname2html(csnames[group]);
fprintf(outfile,"</%s>",csnames[group]);


/*:19*/
#line 284 "./latex2html.w"

}
continue;
break;

/*:16*/
#line 201 "./latex2html.w"

/*17:*/
#line 290 "./latex2html.w"

case COMMENT:
if(c=='\n'){
mode= TEXT;
}
break;

/*:17*/
#line 202 "./latex2html.w"

}
}

/*:11*/
#line 75 "./latex2html.w"

return(0);
}

/*:1*//*6:*/
#line 107 "./latex2html.w"

void new_csname(int label,char tex[],char html[])
{
strcpy(csname_dict[label].latex_name,tex);
strcpy(csname_dict[label].html_name,html);
return;
}

/*:6*//*21:*/
#line 321 "./latex2html.w"

void csname2html(char currentcsname[])
{
int i;
for(i= 0;i<MAX_CSNAMES;++i){
if(strcmp(currentcsname,csname_dict[i].latex_name)==0){
strcpy(currentcsname,csname_dict[i].html_name);
}
}
return;
}
/*:21*/
