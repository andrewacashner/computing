#include<stdio.h>
int b,c;main(){while((c=getchar())!='\n'){  if(!b){if(c>90)c-=32;++b;}else{if(c<97)c+=32;--b;}putchar(c);}puts("");}
