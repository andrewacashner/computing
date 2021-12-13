/* meiHeader.cc
 * Andrew Cashner, 2021/12/13
 *
 * Extract the \header{} from a Lilypond file and translate it to an MEI
 * meiHead element.
 */

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

std::string file_to_string(std::string filename) {
    std::string output_str;
    std::ifstream infile(filename);
    if (infile) {
        std::ostringstream file_str;
        file_str << infile.rdbuf();
        output_str = file_str.str();
    }
    return(output_str);
}

std::string balanced_delimiter_string(std::string source,
        char start_delim, char end_delim) {
    
    std::string output_str = "";
    int start_index = source.find(start_delim);
    int expr_length;
    bool found = false;

    if (start_index != std::string::npos) {
        int brace_level = 0;
        for (int i = start_index; (i < source.length()) && !found; ++i) {
            if (source[i] == start_delim) {
                ++brace_level;
            } else if (source[i] == end_delim) {
                --brace_level;
                if (brace_level == 0) {
                    expr_length = i - start_index + 1;
                    found = true;
                }
            }
        }
        if (found) {
            output_str = source.substr(start_index, expr_length);
        }
    }
    return output_str;    
}

int main(int argc, char **argv) {
    std::string infile_str = "";
    std::string header, mei;

    // Check input argument
    if (argc != 2) {
    std::cerr << "Usage: mei_header INFILE.ly" << std::endl;
        exit(EXIT_FAILURE);
    }
    // Read input
    infile_str = file_to_string(argv[1]);
    
    // Find \header{}
    header = balanced_delimiter_string(infile_str, '{', '}');

    // Parse header
    // Create MEI
    mei = header;

    // Print output
    std::cout << mei << std::endl;
}
