#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>

void *hello(void *recipient)
{
    char *recipient_str = (char *)recipient;
    printf("Hello, %s!\n", recipient_str);

    int length = strlen(recipient_str);

    int *ret = (int *)malloc(sizeof(int));
    if (ret == NULL)
    {
        fprintf(stderr, "Bad malloc\n");
        exit(EXIT_FAILURE);
    }

    *ret = length;

    pthread_exit(ret);
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: pthread NAME\n");
        exit(EXIT_FAILURE);
    }

    char *name = argv[1];

    pthread_t id;

    int result = pthread_create(&id, NULL, hello, name);
    if (result != 0)
    {
        fprintf(stderr, "Thread creation error\n");
        exit(EXIT_FAILURE);
    }

    void *ret;
    result = pthread_join(id, &ret);

    if (result != 0)
    {
        fprintf(stderr, "Thread joining error\n");
        exit(EXIT_FAILURE);
    }

    int length = *(int *)ret;
    free(ret); // TODO is this necessary?

    printf("Recipient's name is %d characters long.\n", length);

    return 0;
}
