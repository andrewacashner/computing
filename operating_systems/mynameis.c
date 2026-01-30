#include <stdio.h>

int main(int argc, char *argv[])
{
    if (argc == 2)
    {
        printf("My name is '%s' and my number is %s.\n", argv[0], argv[1]);
    }

    return 0;
}
