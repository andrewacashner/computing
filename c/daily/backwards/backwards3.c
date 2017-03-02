#include<stdio.h>
main(){int i=0,b[256];while((b[i++]=getchar())!=EOF);while(0<i--)putchar(b[i]);}
