#include "mute.h"

void say(char *msg) {
#ifndef MUTE
    printf("%s\n", msg);
#endif
}
