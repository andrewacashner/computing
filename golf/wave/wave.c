#include <stdio.h>
#define P printf
#define A(i) {for(l='a',p=0;l<='z';++l,++p){if(p==i)P("%c",l-32);else P("%c",l);}P("\n");}
int main(){int i,p;char l;for(i=0;i<26;++i)A(i);for(i=24;i>=0;--i)A(i);return 0;}
