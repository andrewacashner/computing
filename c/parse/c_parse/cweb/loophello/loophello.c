#define LOOPMAX 10
/*1:*/
#line 16 "./loophello.w"

#include <stdio.h> 
/*2:*/
#line 26 "./loophello.w"

static const char greeting[]= "Hello, world!";

/*:2*/
#line 18 "./loophello.w"

int main(void)
{
/*3:*/
#line 31 "./loophello.w"

int i;
for(i= 0;i<LOOPMAX;++i){
printf("%d\t%s\n",i,greeting);
}
/*:3*/
#line 21 "./loophello.w"

return(0);
}

/*:1*/
