/* Grocery List
 *
 * Andrew Cashner, 2025/09/21
 *
 * Keep a grocery list sorted by item category.
 * Add to the list at the command line.
 *
 *      grocery --base "$HOME"/.grocery/grocery.txt
 *      grocery --restart 
 *      grocery --add milk
 *      grocery --add eggs --add flour
 *      grocery --add-all butter margarine oil
 *      grocery --remove margarine
 *      grocery --print current_grocery_list.txt
 */

// TODO
// - support add-all
// - use enum for actions stored in Command
// - make hashmap for command -> function

#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;

class Command {
    private:
        enum class Action { 
            BASE, RESET, ADD, ADD_ALL, REMOVE, PRINT 
        };

        vector<string> action_strings { 
            "base", "reset", "add", "add_all", "remove", "print" 
        };

        Action action;
        string arg = "";

    public:
        Command(string action_str, string arg) :arg(arg) { 
            auto match = find(action_strings.begin(), 
                    action_strings.end(), action_str);

            if (match != action_strings.end()) {
                action = (Action) distance(action_strings.begin(), match);
            } else {
                throw invalid_argument("Unrecognized action '" + action_str + "'");
            }
        }

        Command(string action_str) { Command(action_str, ""); }

        Action get_action() { return action; }
        string to_string() { return action_strings[(int) action] + "(" + arg + ")"; }
};

list<Command> parse_args(char **argv);

const string list_filename = "grocery_list.txt";

int main(int argc, char *argv[])
{
    if (argc < 2) {
        cout << "Usage: grocery --COMMAND arg[, arg2]\n";
        return EXIT_FAILURE;
    }

    try {
        // Create and/or open list file
        fstream list_file { list_filename , fstream::in | fstream::out };

        list<Command> commands = parse_args(argv);
        for (Command command : commands) {
            cout << command.to_string() << "\n";
            // command.execute(); TODO
        }
    }
    catch (const invalid_argument& ex) {
        cout << ex.what() << "\n";
        return EXIT_FAILURE;
    }
    // finally { // TODO
    //     // Close list file
    // }

    return 0;
}

list<Command> parse_args(char **argv)
{
    list<Command> commands;

    for (int i = 1; argv[i]; ++i) {
        if (string word = argv[i]; word.starts_with("--")) {

            string next = "";
            if (argv[i + 1] && !((string)argv[i + 1]).starts_with("--")) {
                next = argv[i + 1];
                ++i;
            }
            
            commands.push_back(Command { word.substr(2), next });
        } 
        else {
            throw invalid_argument{ "Invalid argument '" + word + "'" };
        }
    }

    return commands;
}

