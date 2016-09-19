/* Golf: Print an ascii square given two integers in an array */
#include<stdio.h>
#define L(x,y)for(x=y;x>0;--x)
#define T(x)putchar(124);L(w,W)putchar(x);printf("|\n");
S(h,w){int H=h-2,W=w-2;T(45);L(h,H){T(' ');}T(45);}

/* Below: full program to run above function */
#include<stdlib.h>
main(int c,char**v){if(c==3)S(atoi(v[1]),atoi(v[2]));}
