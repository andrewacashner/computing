#include <iostream>
#include <string>

int main(void)
{
    char c_string[] = "Hello";
    if (std::string str = c_string; str.starts_with("He")) {
        std::cout << "True";
    }
    else {
        std::cout << "False";
    }

    return 0;
}

