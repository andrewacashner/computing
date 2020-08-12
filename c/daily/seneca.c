#include <stdio.h>
int main(void) {
    char line[80];
    char name[80];

    printf("Dë'ëh sya:söh?\n");
    fgets(line, sizeof(line), stdin);
    sscanf(line, "%s", name);
    printf("Nya:wëh sge:no, %s!\n", name);
    return(0);
}

