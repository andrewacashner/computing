/*-----------------------------------------------------------------------------
 * dictionary
 *
 * Andrew Cashner
 * 2026/09/15
 *
 * A dictionary or hash map data structure
 * -----------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_INPUT                   (500)

#define MAX_KEY                     (100)
#define MAX_VALUE                   (500)
#define MAX_DICT_ENTRIES            (100)

#define DICT_ADD_SUCCESS            (0)
#define DICT_ADD_FAILURE_DICT_FULL  (1)

typedef struct 
{
    char m_key[MAX_KEY];
    char m_value[MAX_VALUE];
} Dict_entry;

typedef struct 
{
    int m_size;
    Dict_entry m_entries[MAX_DICT_ENTRIES];
} Dict;

void Dict_create(Dict *dict);
int Dict_add(Dict *dict, char *key, char*value);
int Dict_size(Dict *dict);

int main(void) 
{

    Dict dict;
    Dict_create(&dict);
    
    printf("Create a dictionary\n");
    char input[MAX_INPUT];

    bool finished = false;

    while (!finished)
    {
        char key[MAX_KEY];
        char value[MAX_VALUE];

        printf("Enter a key (or 'q' after last entry): ");
        if (fgets(input, sizeof(input), stdin) == NULL) {
            fprintf(stderr, "Problem reading input\n");
            exit(EXIT_FAILURE);
        } else {
            sscanf(input, "%s", key);
        }
        
        if (strcmp(key, "q") == 0) {
            finished = true;
            continue;
        } 
        
        printf("Enter a value for  key '%s': ", key);
        if (fgets(input, sizeof(input), stdin) == NULL) {
            fprintf(stderr, "Problem reading input\n");
            exit(EXIT_FAILURE);
        } else {
            sscanf(input, "%s", value);
        }
        
        printf("Adding new entry:\n    '%s': '%s'\n\n", key, value);

        int result = Dict_add(&dict, key, value);

        if (result == DICT_ADD_SUCCESS) 
        {
            printf("Operation succeeded.\n");
        } 
        else 
        {
            printf("Operation failed with error code %d.\n", result);
        }
    }

    int entries = Dict_size(&dict);
    printf("Dictionary has %d entries\n", entries);

    return 0;
}

void Dict_create(Dict *dict)
{
    dict->m_size = 0;
    // Nothing to do when dict is on the stack
}

int Dict_add(Dict *dict, char *key, char*value)
{
    int status = DICT_ADD_SUCCESS;

    if (dict->m_size < MAX_DICT_ENTRIES) {
        Dict_entry new_entry = dict->m_entries[dict->m_size];
        strncpy(new_entry.m_key, key, MAX_KEY);
        strncpy(new_entry.m_value, value, MAX_VALUE);

        ++dict->m_size;
    } else {
        status = DICT_ADD_FAILURE_DICT_FULL;
    }
    
    return status;
}

int Dict_size(Dict *dict)
{
    return dict->m_size;
}

// TODO Dict_get
