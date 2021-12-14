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
#include <map>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
#include <mei/mei.h>
#include <mei/header.h>
#include <mei/shared.h>
#include <mei/xmlexport.h>
#pragma clang diagnostic pop

/* Read the contents of a file into a string. */
std::string file_to_string(std::string filename) {
    std::string output_str;
    std::ifstream infile(filename);
    if (infile) {
        std::ostringstream file_str;
        file_str << infile.rdbuf();
        output_str = file_str.str();
    }
    return output_str;
}

/* TRIM WHITESPACE */
const std::string kWhitespace = " \r\n\t\f\v";

/* Trim from left side of string */
std::string trim_left(std::string source) {
    std::string output_str = source;
    size_t end_index = source.find_first_not_of(kWhitespace);
    if (end_index != std::string::npos) {
        output_str = source.erase(0, end_index);
    }
    return output_str;
}

/* Trim from right side of string */
std::string trim_right(std::string source) {
    std::string output_str = source;
    size_t start_index = source.find_last_not_of(kWhitespace);
    if (start_index != std::string::npos) {
        output_str = source.erase(start_index + 1);
    } else {
        output_str = "";
    }
    return output_str;
}

/* Trim whitespace from both ends of a string. */
std::string trim(std::string source) {
    return (trim_right(trim_left(source)));
}

/* EXTRACT PORTIONS OF STRINGS */

/* Copy the portion of a string within balanced delimiters such as curly
 * brackets, including any nested delimited strings. Delimiters are included
 * in the output string. */
std::string balanced_delimiter_substring(std::string source,
        char start_delim, char end_delim) {
    
    std::string output_str = "";
    size_t start_index = source.find(start_delim);
    size_t end_index, expr_length;

    if (start_index != std::string::npos) {
        int brace_level = 0;
        size_t end_index = start_index;
        for (auto &c : source) {
            if (c == start_delim) {
                ++brace_level;
            } else if (c == end_delim) {
                --brace_level;
                if (brace_level == 0) {
                    size_t expr_length = end_index - start_index + 1;
                    output_str = source.substr(start_index, expr_length);
                    break;
                }
            }
            ++end_index;
        }
    }
    return output_str;
}

/* Find substring delimited by curly braces. */
std::string brace_delimited_substring(std::string source) {
    return (balanced_delimiter_substring(source, '{', '}'));
}

/* Find the position starting after the given substring */
size_t find_first_after_substring(std::string source, std::string substring) {
    size_t index = source.find(substring);
    if (index != std::string::npos) {
        index += substring.length();
    }
    return index;
}

/* Given a Lilypond command (e.g., \markup {}) extract its
 * curly-brace-delimited argument. */
std::string ly_brace_argument(std::string source, std::string command) {
    std::string arg = "";
    size_t start_index = find_first_after_substring(source, command);
    if (start_index != std::string::npos) {
        arg = brace_delimited_substring(
                source.substr(start_index, std::string::npos));
    }
    return arg;
}

std::string drop_before(std::string source, std::string prefix) {
    std::string output_str = source;
    size_t index = find_first_after_substring(source, prefix);
    if (index != std::string::npos) {
        output_str = source.erase(0, index);
    }
    return output_str;
}

std::string drop_after(std::string source, std::string suffix) {
    std::string output_str = source;
    size_t index = source.find(suffix);
    if (index != std::string::npos) {
        output_str = source.erase(index);
    }
    return output_str;
}

/* Copy the portion of a string between quotation marks ('"'). */
std::string quoted_substring(std::string source) {
    std::string output_str = "";
    if (source.find('"') != std::string::npos) {
        std::string after_quote = drop_before(source, "\"");
        output_str = drop_after(after_quote, "\"");
    }
    return output_str;
}

std::string concat_quoted_substrings(std::string source) {
    std::string output_str = "";
    while (source.find('"') != std::string::npos) {
        std::string this_substring = quoted_substring(source);
        if (!this_substring.empty()) {
            output_str += " " + this_substring;
            source = drop_before(source, "\"" + this_substring + "\""); 
        }
    }
    return trim(output_str);
}



/* DICTIONARY FOR HEADER */
using dictionary = std::map<std::string, std::string>;

bool starts_with(std::string source, std::string start_string) {
    return (source.find(start_string) == 0);
}

/* Find a Lilypond \header{} and return a dictionary with the keys and values
 * that were defined within it. */
dictionary header_to_dict(std::string source) {
    dictionary header_dict;
    std::istringstream source_stream(source);
    std::string this_line;
    size_t this_line_index = 0;
    while (std::getline(source_stream, this_line)) {
        size_t start_index = this_line.find(" = ");
        if (start_index != std::string::npos) {
            std::string key = trim(this_line.substr(0, start_index));
            std::string value = this_line.substr(start_index + 3, std::string::npos);

            std::string markup_cmd = "\\markup ";
            if (starts_with(trim_left(value), markup_cmd)) {
                std::string test = source.substr(this_line_index, std::string::npos);
                value = concat_quoted_substrings(ly_brace_argument(test, markup_cmd));

                this_line_index += find_first_after_substring(this_line, markup_cmd) 
                                    + value.length();
            } else {
                value = quoted_substring(value);
                this_line_index += this_line.length();
            }

            header_dict.emplace(key, value);
        }
    }
    return header_dict;
}

/* Create the MEI meiHead element from the contents of the header dictionary.
 * */
mei::MeiHead* create_header(dictionary dict) {
    mei::MeiHead *meiHead = new mei::MeiHead();
    
    mei::TitleStmt *titleStmt = new mei::TitleStmt();
    meiHead->addChild(titleStmt);

    auto iter = dict.find("title");
    if (iter != dict.end()) {
        
        mei::MeiElement *title = new mei::MeiElement("title");
        mei::MeiAttribute *title_type = new mei::MeiAttribute("type", "main");
        title->addAttribute(title_type);

        title->setValue(iter->second);
        titleStmt->addChild(title);
    }

    mei::RespStmt *respStmt = new mei::RespStmt();
    meiHead->addChild(respStmt);

    iter = dict.find("composer");
    if (iter != dict.end()) {
        mei::MeiElement *composer = new mei::MeiElement("composer");
        composer->setValue(iter->second);
        respStmt->addChild(composer);
    }

    iter = dict.find("poet");
    if (iter != dict.end()) {
        mei::MeiElement *lyricist = new mei::MeiElement("lyricist");
        lyricist->setValue(iter->second);
        respStmt->addChild(lyricist);
    }
    // TODO add more header fields
    return meiHead;
}

/* Create the MEI music element. */
mei::Music* create_music(void) { // TODO for now
    mei::Music *music = new mei::Music();

    mei::Body *body = new mei::Body();
    music->addChild(body);

    mei::Mdiv *mdiv = new mei::Mdiv();
    body->addChild(mdiv);

    mei::Score *score = new mei::Score();
    mdiv->addChild(score);

    mei::ScoreDef *scoreDef = new mei::ScoreDef();
    score->addChild(scoreDef);

    return music;
}


int main(int argc, char **argv) {
    // Check input argument
    if (argc != 3) {
    std::cerr << "Usage: mei_header INFILE.ly OUTFILE.xml" << std::endl;
        exit(EXIT_FAILURE);
    }
    // Read input
    std::string infile_str = "";
    std::string infile_name = argv[1];
    std::string outfile_name = argv[2];
    infile_str = file_to_string(infile_name);

    // Find \header{}
    std::string header = balanced_delimiter_substring(infile_str, '{', '}');

    // Parse header 
    dictionary header_dict = header_to_dict(header);
    
    // Create MEI header
    mei::MeiHead *meiHead = create_header(header_dict);

    // Create MEI doc
    mei::MeiDocument *doc = new mei::MeiDocument();
    mei::MeiElement *m1 = new mei::MeiElement("mei");
    doc->setRootElement(m1);
    m1->addChild(meiHead);

    mei::Music *music = create_music();
    m1->addChild(music);

    // Print output
    bool status = mei::documentToFile(doc, outfile_name);
    if (!status) {
        std::cerr << "Error: Could not write output" << std::endl;
        exit(EXIT_FAILURE);
    }
}
