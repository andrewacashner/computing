/* Grocery List
 *
 * Andrew Cashner, 2025/09/21
 *
 * Keep a grocery list sorted alphabetically.
 * Modify the list at the command line.
 *
 *      grocery --list "$HOME"/.grocery/grocery.txt
 *      grocery --reset
 *      grocery --add milk
 *      grocery --add eggs --add flour
 *      grocery --remove margarine
 *      grocery --print 
 *      grocery --list party.txt --add seltzer --add cupcakes --print
 *      grocery --add chicken --list birthday.txt --add 'chocolate chips'
 */

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <list>

using std::string;

class Command {
    public:
        enum class Action { LIST, RESET, ADD, REMOVE, PRINT };

    private:
        std::vector<string> words { "list", "reset", "add", "remove", "print" };
       
        Action action;
        string arg { "" };

    public:
        Command(string word, string arg);
        Command(string action_str) { Command(action_str, ""); }

        Action get_action() { return action; }
        string get_arg()    { return arg; }

        string to_string()  { return words[(int) action] + "(" + arg + ")"; }
};

class GroceryList {
    private:
        std::list<string> items;
        string source_file { "grocery_list.txt" };

        void load();
        void save();
        bool contains(string);
        void set_list(string);
        void reset();
        void add(string);
        void remove(string);
        void print();

    public:
        GroceryList()   { load(); }
        ~GroceryList()  { save(); }

        void execute(Command& cmd);
};

std::vector<Command> parse_args(char **argv);


int main(int argc, char *argv[])
{
    if (argc < 2) {
        std::cout << "Usage: grocery --COMMAND arg[, arg2]\n";
        return EXIT_FAILURE;
    }

    try {
        GroceryList grocery_list;

        std::vector<Command> commands = parse_args(argv);
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


std::vector<Command> parse_args(char **argv)
{
    std::vector<Command> commands;

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

template<typename S, typename O>
int index_of(const S& seq, const O& obj)
{
    auto match = std::find(seq.begin(), seq.end(), obj);
    return (match != seq.end()) ? std::distance(seq.begin(), match) : -1;
}

template<typename S, typename O>
bool contains(const S& seq, const O& obj) { return index_of(seq, obj) != -1; }

Command::Command(string word, string arg) :arg(arg) 
{ 
    if (int index = index_of(words, word); index != -1) {
        action = (Action) index;
    }
    else throw std::invalid_argument{ "Unrecognized action '" + word + "'" };
}


void GroceryList::load()
{
    items.clear();

    std::fstream list_file { source_file, std::fstream::in };

    string line;
    while (std::getline(list_file, line)) {
        items.push_back(line);
    }

    list_file.close();
}

void GroceryList::save()
{
    std::fstream list_file { source_file, 
        std::fstream::out | std::fstream::trunc };

    items.sort();

    for (string item : items) {
        list_file << item << "\n";
    }

    list_file.close();
}

bool GroceryList::contains(string item) { return ::contains(items, item); }

void GroceryList::set_list(string filename)
{
    source_file = filename;
    load();
}

void GroceryList::reset() { items.clear(); }

void GroceryList::add(string item)
{
    if (contains(item)) {
        std::cout << "Item '" << item << "' already on list\n";
    }
    else {
        items.push_back(item);
        save();
    }
}

void GroceryList::remove(string item)
{
    if (!contains(item)) {
        std::cout << "Item '" << item << "' not on list; could not remove\n";
    }
    else {
        items.remove(item);
        save();
    }
}

void GroceryList::print()
{
    std::cout << "\nGrocery List\n" << "------------\n";
    for (string item : items) {
        std::cout << item << "\n";
    }
}

void GroceryList::execute(Command& cmd)
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

