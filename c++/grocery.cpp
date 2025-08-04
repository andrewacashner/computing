/* Grocery List
 *
 * Andrew Cashner, 2025/09/21
 *
 * Keep a grocery list sorted alphabetically.
 * Modify the list at the command line.
 *
 *      grocery --list "$HOME"/.grocery/grocery.txt
 *      grocery --restart 
 *      grocery --add milk
 *      grocery --add eggs --add flour
 *      grocery --remove margarine
 *      grocery --print 
 *      grocery --list party.txt --add seltzer --add cupcakes --print
 */

#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <vector>
#include <algorithm>
#include <iterator>
#include <functional>

using std::string;

class Command {
    public:
        enum class Action { LIST, RESET, ADD, REMOVE, PRINT };

    private:
        std::vector<string> words { "list", "reset", "add", "remove", "print" };
       
        Action action;
        string arg { "" };

    public:
        Command(string word, string arg) :arg(arg) 
        { 
            auto match = find(words.begin(), words.end(), word);

            if (match != words.end()) {
                action = (Action) std::distance(words.begin(), match);
            } 
            else throw std::invalid_argument{ "Unrecognized action '" + word + "'" };
        }

        Command(string action_str) { Command(action_str, ""); }

        Action get_action() { return action; }
        string get_arg()    { return arg; }

        string to_string()  { return words[(int) action] + "(" + arg + ")"; }
};

class GroceryList {
    private:
        std::list<string> items;
        string source_file { "grocery_list.txt" };

        bool contains(string item) {
            return (std::find(items.begin(), items.end(), item) != items.end());
        }

        void set_list(string filename)
        {
            source_file = filename;
            load();
        }

        void reset() { items.clear(); }

        void add(string item)
        {
            if (contains(item)) {
                std::cout << "Item '" << item << "' already on list\n";
            }
            else {
                items.push_back(item);
                save();
            }
        }

        void remove(string item)
        {
            if (!contains(item)) {
                std::cout << "Item '" << item << "' not on list; could not remove\n";
            }
            else {
                items.remove(item);
                save();
            }
        }

        void print()
        {
            std::cout << "\nGrocery List\n" << "------------\n";
            for (string item : items) {
                std::cout << item << "\n";
            }
        }

    public:
        GroceryList()   { load(); }
        ~GroceryList()  { save(); }

        void load()
        {
            items.clear();

            std::fstream list_file { source_file, std::fstream::in };

            string line;
            while (std::getline(list_file, line)) {
                items.push_back(line);
            }

            list_file.close();
        }

        void save()
        {
            std::fstream list_file { source_file, 
                std::fstream::out | std::fstream::trunc };
          
            items.sort();

            for (string item : items) {
                list_file << item << "\n";
            }
            
            list_file.close();
        }


        void execute(Command& cmd)
        { 
            switch (cmd.get_action()) {
                case Command::Action::LIST:
                    set_list(cmd.get_arg());
                    break;

                case Command::Action::RESET:
                    reset();
                    break;

                case Command::Action::ADD:
                    add(cmd.get_arg());
                    break;

                case Command::Action::REMOVE:
                    remove(cmd.get_arg());
                    break;

                default: // Command::Action::PRINT:
                    print();
                    break;
            }
        }
};

std::list<Command> parse_args(char **argv);

int main(int argc, char *argv[])
{
    if (argc < 2) {
        std::cout << "Usage: grocery --COMMAND arg[, arg2]\n";
        return EXIT_FAILURE;
    }

    try {
        GroceryList grocery_list;

        std::list<Command> commands = parse_args(argv);
        for (Command command : commands) {
            // cout << command.to_string() << "\n";
            grocery_list.execute(command);
        }
    }

    catch (const std::invalid_argument& ex) {
        std::cerr << ex.what() << "\n";
        return EXIT_FAILURE;
    }

    return 0;
}

std::list<Command> parse_args(char **argv)
{
    std::list<Command> commands;

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
            throw std::invalid_argument{ "Invalid argument '" + word + "'" };
        }
    }

    return commands;
}

