/* odds.cc -- Andrew Cashner, 2017/04/03
 * Print odd numbers from 1 to user-given limit
 */

#include <iostream>

int main() {
    int lim = 0;
    std::cout << "Enter limit: ";
    std::cin >> lim;
    for (int num = 1; num <= lim; ++num) {
        if (num % 2 == 1) {
            std::cout << num << " ";
        }
        if (num % 10 == 0) { // new line for each tens group
            std::cout << std::endl;
        }
    }
    std::cout << std::endl;
    return 0;
}
