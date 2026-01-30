#include <stdio.h>
#include <unistd.h>

void wait(char c);

int main(void)
{
    printf("A ");
    wait('a');

    int pid = fork();
    printf("%d ", pid);

    if (pid != 0)
    {
        // in parent
        printf("B ");
        wait('b');
       
        printf("E ");
        int pid = fork();
        printf("%d ", pid);
        wait('e');
    }
    else
    {
        // in child
        printf("C ");
        wait('c');
       
        printf("D ");
        int pid = fork();
        printf("%d ", pid);
        wait('d');
    }

    return 0;
}

void wait(char c)
{
    for (int i = 0; i < 3; ++i)
    {
        printf("%c ", c);
        fflush(stdout);
        sleep(1);
    }
}
