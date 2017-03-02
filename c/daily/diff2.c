#include<stdio.h>
#define L(x)for(i=0;i<x;++i)
main(int C,char**V){int A[3],i,j,m,t;L(3)sscanf(V[i+1],"%d",&A[i]);
L(2){m=i;for(j=i+1;j<3;++j)if(A[j]>A[m])m=j;t=A[i],A[i]=A[m],A[m]=t;}
j=A[0],m=A[1],t=A[2];printf("%s\n",(i=(j-m==t||j-t==m)?0:1)?"False":"True");}
    
