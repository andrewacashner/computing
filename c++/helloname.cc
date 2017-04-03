/* helloname.cc -- Andrew Cashner, 2017/04/03
 * Take name from standard input and greet user with that name.
 */
#include <iostream>

#define MAX_CHAR 80

int main() {
    char name[MAX_CHAR];
    std::cout << "What is your name? ";
    std::cin >> name;
    std::cout << "Hello, " << name << "!" << std::endl;
    return 0;
}
