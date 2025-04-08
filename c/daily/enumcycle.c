#include <stdio.h>

#define MAX_ACTIVITY 3

typedef enum {
    SLEEP,
    WORK,
    PLAY
} Activity_t;

char *activity_description[] = {
    "sleep",
    "work",
    "play"
};

char *activity_location[] = {
    "home",
    "office",
    "gym"
};

Activity_t activity_next(Activity_t current) {
    return ++current % MAX_ACTIVITY;
}

void activity_print(Activity_t activity) {
    printf("%s at %s\n", activity_description[activity],
                    activity_location[activity]);
}

int main(void) {
    Activity_t activity = SLEEP;

    printf("Current activity: ");
    activity_print(activity);
    printf("Enter 'n' to advance to the next activity, or 'q' to quit.\n");
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
