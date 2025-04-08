#include <stdio.h>

#define MAX_ACTIVITY 3

typedef enum {
    SLEEP,
    WORK,
    PLAY
} Activity_label_t;

typedef struct Activity_t *Activity_ptr;

typedef struct Activity_t {
    Activity_label_t label;
    char *description;
    char *location;
    Activity_ptr next;
} Activity_t;

Activity_t actions[] = {
    { 
        .label =         SLEEP,
        .description =   "sleep",
        .location =      "home",    
        .next =          &actions[WORK]
    },
    {
        .label =         WORK,
        .description =   "work",
        .location =      "office",
        .next =          &actions[PLAY]
    },
    {
        .label =         PLAY,
        .description =   "play",
        .location =      "gym",
        .next =          &actions[SLEEP]
    }
};

Activity_ptr action(Activity_label_t label) {
    return &actions[label];
}

Activity_ptr activity_next(Activity_ptr current) {
    return current->next;
}

void activity_print(Activity_ptr activity) {
    printf("%s at %s\n", activity->description, activity->location);
}

int main(void) {
    Activity_ptr activity = action(SLEEP);

    printf("Current activity:\n");
    activity_print(activity);
    printf("\nEnter 'n' to advance to the next activity, or 'q' to quit.\n");
    int c;
    while ((c = getchar()) != 'q') {
        if (c == 'n') {
            activity = activity_next(activity);
        }
        if (c != '\n') {
            activity_print(activity);
        }
    }

    return 0;
}
