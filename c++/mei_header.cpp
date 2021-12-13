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
#include <mei/mei.h>
#include <mei/header.h>
#include <mei/shared.h>
#include <mei/xmlexport.h>

/* Read the contents of a file into a string. */
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

/* Copy the portion of a string within balanced delimiters such as curly
 * brackets, including any nested delimited strings. Delimiters are included
 * in the output string. */
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
    return(output_str);
}

/* Copy the portion of a string between quotation marks ('"'). */
std::string copy_quoted_string(std::string source) {
    std::string output_str = "";
    int start_index = source.find('"');
    if (start_index != std::string::npos) {
        int end_index = source.find('"', start_index + 1);
        if (end_index != std::string::npos) {
            output_str = source.substr(start_index + 1, 
                             end_index - start_index - 1);
        }
    }
    return(output_str);
}

/* Trim whitespace from both ends of a string. */
std::string trim_whitespace(std::string source) {
    const std::string whitespace = " \r\n\t\f\v";
    int start_index = source.find_first_not_of(whitespace);
    if (start_index != std::string::npos) {
        source.erase(0, start_index);
        int end_index = source.find_last_not_of(whitespace);
        if (end_index != std::string::npos) {
            source.erase(end_index + 1);
        } else {
            source.clear();
        }
    }
    return(source);
}

/* Dictionary for header */
using dictionary = std::map<std::string, std::string>;

/* Find a Lilypond \header{} and return a dictionary with the keys and values
 * that were defined within it. */
dictionary header_to_dict(std::string source) {
    dictionary header_dict;
    std::istringstream source_stream(source);
    std::string this_line;
    while (std::getline(source_stream, this_line)) {
        int start_index = this_line.find(" = ");
        if (start_index != std::string::npos) {
            std::string key = trim_whitespace(this_line.substr(0, start_index));
            std::string value = copy_quoted_string(this_line.substr(
                                    start_index + 3, std::string::npos));
            header_dict.emplace(key, value);
        }
    }
    return(header_dict);
}

/* Create the MEI meiHead element from the contents of the header dictionary.
 * */
mei::MeiHead* create_header(dictionary dict) {
    mei::MeiHead *meiHead = new mei::MeiHead();
    
    auto iter = dict.find("title");
    if (iter != dict.end()) {
        mei::TitleStmt *titleStmt = new mei::TitleStmt();
        
        mei::MeiElement *title = new mei::MeiElement("title");
        mei::MeiAttribute *title_type = new mei::MeiAttribute("type", "main");
        title->addAttribute(title_type);

        title->setValue(iter->second);
        titleStmt->addChild(title);
        meiHead->addChild(titleStmt);
    }

    iter = dict.find("composer");
    if (iter != dict.end()) {
        mei::RespStmt *respStmt = new mei::RespStmt();
       
        mei::MeiElement *composer = new mei::MeiElement("composer");
        composer->setValue(iter->second);
       
        respStmt->addChild(composer);
        meiHead->addChild(respStmt);
    }

    return(meiHead);
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

    return(music);
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
    std::string header = balanced_delimiter_string(infile_str, '{', '}');

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
