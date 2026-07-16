/* DIY tree utility (C++)
 *
 * Andrew Cashner
 * 2026/07/16
 */

#include <iostream>
#include <string>
#include <filesystem>

void print_dir(std::string path);

int main(int argc, char *argv[]) 
{
    if (argc != 2)
    {
        std::cerr << "Usage: tree PATH\n";
        std::exit(EXIT_FAILURE);
    }

    std::string path { argv[1] };
    print_dir(path);

    return 0;
}

void print_dir(std::string path)
{
    std::cout << path << "\n";

    auto iterator = std::filesystem::recursive_directory_iterator(path);

    for (const std::filesystem::directory_entry& entry : iterator)
    {
        std::string indent = std::string(2 * iterator.depth(), ' ');
        std::string basename { entry.path().filename() };

        std::cout << indent << "|__ " << basename << "\n";
    }
}

