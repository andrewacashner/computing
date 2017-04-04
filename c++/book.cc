/* book.cc -- Andrew A. Cashner, 2017/04/04
 * First try at a class
 */

#include <iostream>
#include <string>
#include <vector>
using namespace std;

class Book {
    public: 
        Book() = default;
        Book(int n, const string &a, const string &t, double p) :
            id(n), author(a), title(t), price_usd(p) { }
        Book(istream &);
    
        ostream &print() const {
            return cout << "Book " << id << ": "
                << author << ", " << title
                << " (" << price_usd << ")";
        }
    private:
        int id;
        string author;
        string title;
        double price_usd;
};

int main() {
    vector<Book> library = {
        {23, "Athanasius Kircher", "Musurgia universalis", 999.95},
        {37, "Juan de Palafox y Mendoza", "Bocados espirituales", 362.47},
        {12, "Steve Oualline", "Practical C Programming", 29.95}
    };
    for (auto i = library.cbegin(); i != library.cend(); ++i) {
        (*i).print() << endl;
    }

    return 0;
}
