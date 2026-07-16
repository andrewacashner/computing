/* DIY tree utility
 *
 * Andrew Cashner
 * 2026/07/16
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <limits.h>

void print_dir(const char *path);

int main(int argc, char *argv[]) 
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: tree PATH\n");
        exit(EXIT_FAILURE);
    }

    const char *path = argv[1];
    print_dir(path);

    return 0;
}

void print_path(const char *path, int indent)
{
    char *spaces = "  ";
    for (int i = 0; i < indent - 1; ++i) 
    {
        printf("%s", spaces);
    }

    char *preface = (indent > 0) ? "|__ " : "";
    printf("%s%s\n", preface, path);
}


void do_print_dir(const char *full_path, const char *basename, int indent)
{
    print_path(basename, indent);

    DIR *directory = opendir(full_path); 

    if (directory)
    {
        char path_buffer[PATH_MAX] = "";

        struct dirent *content;
        while ((content = readdir(directory)) != NULL) 
        {
            if (content->d_name[0] != '.')
            {
                sprintf(path_buffer, "%s/%s", full_path, content->d_name);
                do_print_dir(path_buffer, content->d_name, indent + 1);
            }
        } // TODO report error if there is one

        int result = closedir(directory);
        if (result != 0)
        {
            fprintf(stderr, "Problem closing directory\n");
            // TODO report error from errno
        }
    } 
}


void print_dir(const char *path) {
    do_print_dir(path, path, 0);
}
