#include<stdio.h>
#include<string.h>
main(){int i,j,c;char l[256],*L[256];
  for(i=0;(c=getchar())!=EOF;++i){fgets(l,sizeof(l),stdin);strcpy(L[i],l);}
  for(;i>=0;--i){printf("%s",L[i]);}}
