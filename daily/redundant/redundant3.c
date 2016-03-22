#define I {4,2,3,1,3,12,2,27,4}
#define M 9

#include<stdio.h>
#define P printf
#define L(a,b,c) for(a=b;a<c;++a)
#define O(d,e) L(i,0,e){P("%d",d[i]);i<e-1?P(", "):0;}
int i,j,t,x,u[M],a[]=I;
main(){
L(i,0,M){x=a[i];L(j,i+1,M)x==a[j]?u[t++]=x:0;}
P("[");O(a,M)P("] => [");O(u,t)P("]\n");
}


