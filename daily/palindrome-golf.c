#include<stdio.h>
#define M m[i]
int i,j,m[96];main(){
for(;(M=getchar())!='\n';++i)M>96?M-=32:0;
for(j=--i,i=0;i<j;++i,--j){if(M>64){if(M!=m[j])exit(9);}}return;}
    
