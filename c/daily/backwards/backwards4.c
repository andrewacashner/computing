#include<stdio.h>
main(){int i=16,b[16];for(;((b[i]=getchar())!=EOF);--i);
for(;i>=0;--i)putchar(b[i]);}
while(i>=0)putchar(b[i--]);}
