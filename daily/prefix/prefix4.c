#define A "global"
#define B "glossary"

#include<stdio.h>
main(){char a[]=A,b[]=B,*x=a,*y=b;while(*x++==*y++);*--x='\0';printf("%s\n",a);}

    
