#include <stdio.h>
#include <unistd.h>

int main(void)
{
    int pid = fork();
    char pid_str[10];
    sprintf(pid_str, "%d", pid);

    if (pid == 0)
    {
        char *argv[] = { "child", pid_str, NULL };
        execvp("./build/mynameis.cx", argv);
    }
    else
    {
        char *argv[] = { "parent", pid_str, NULL };
        execvp("./build/mynameis.cx", argv);
    }

    printf("Processes not replaced\n");

    return 0;
}
