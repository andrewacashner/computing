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

#include <iostream>
#include <string>
#include <list>

using namespace std;

class Command {
    private:
        string action;
        string arg = "";

    public:
        Command(string action, string arg) :action(action), arg(arg) {}
        Command(string action) :action(action) {} 
        void set_arg(string new_arg) { arg = new_arg; }
        string to_string() { return action + "(" + arg + ")"; }
};

list<Command> parse_args(char **argv);

int main(int argc, char *argv[])
{
    if (argc < 2) {
        cout << "Usage: grocery --COMMAND arg[, arg2]\n";
        return EXIT_FAILURE;
    }

    try {
        list<Command> commands = parse_args(argv);
        for (Command command : commands) {
            cout << command.to_string() << "\n";
        }
    }
    catch (const invalid_argument& ex) {
        cout << ex.what() << "\n";
        return EXIT_FAILURE;
    }

    return 0;
}

list<Command> parse_args(char **argv)
{
    list<Command> commands;

    for (int i = 1; argv[i]; ++i) {
        if (string arg = argv[i]; arg.starts_with("--")) {
            Command this_command { arg.substr(2) };

            if (argv[i + 1]) {
                string next = argv[i + 1];
                if (!next.starts_with("--")) {
                    this_command.set_arg(next);
                    ++i;
                }
            }

            commands.push_back(this_command);
        } 
        else throw invalid_argument{ "Invalid argument '" + arg + "'" };
    }

    return commands;
}

