\let\ifpdf\relax
\input eplain

\centerline{\titlefont Loop hello}
\centerline{\today}

@ This is \.{loophello}, a \.{CWEB} program that writes a greeting multiple times.

\unorderedlist
\li First we define a greeting message.
\li Second we set up a loop to repeat the message.
\li Then we return and exit.
\endunorderedlist


@p
#include <stdio.h>
@< Define the greeting message. @>@;
int main(void)
{
	@< Print greeting multiple times. @>@;
	return(0);
}

@ We define the greeting as a constant character array.
@< Define the greeting... @>=
static const char greeting[] = "Hello, world!";

@ We use a loop to print the greeting and the index number up to \.{LOOPMAX}.
@d LOOPMAX 10
@< Print greeting... @>=
int i;
for (i = 0; i < LOOPMAX; ++i) {
	printf("%d\t%s\n", i, greeting);
}

