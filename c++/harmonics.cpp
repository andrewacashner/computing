/* harmonics.cc
 * Andrew A. Cashner, 2021/08/20
 * Print a table of the harmonic series above a given fundamental frequency in Hz
 */

#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
using namespace std;

class Harmonic {
    public:
        Harmonic() = default;
        Harmonic(int fundamental, int factor):
            frequency_hz(get_frequency(fundamental, factor)),
            factor(factor), 
            denominator(get_denominator(factor)),
            ratio_str(get_ratio_str(factor))
            { }
        Harmonic(istream &);

        ostream &print() const {
            return print_table_row(to_string(factor), ratio_str, to_string(frequency_hz));
        }
        ostream &print_header() const {
            return print_table_row("Factor", "Ratio", "Frequency (Hz)") 
                    << endl << string(35, '-'); 
        }

    private: 
        int frequency_hz;
        int factor;
        int denominator;
        string ratio_str;

        int get_frequency(int fundamental, int factor) {
            return fundamental * factor;
        }
        // Ratio is factor to (factor - 1), unless factor is 1 (then "1:1")
        int get_denominator(int factor) {
            return factor == 1 ? 1 : factor - 1;
        }
        string get_ratio_str(int factor) {
            return to_string(factor) + ":" + to_string(denominator);
        }
        ostream &print_table_row(string a, string b, string c) const {
            return cout << setfill(' ')
                        << setw(5) << a 
                        << setw(10) << b
                        << setw(20) << c;
        }

};

const string usage = "Usage: harmonics FREQ MAX\n"
                     "       FREQ: Frequency of fundamental (Hz)\n"
                     "       MAX:  Number of partials to print";

void exit_error_msg(string msg);

int main(int argc, char *argv[]) {
    // Check arguments
    if (argc != 3) {
        exit_error_msg(usage);
    }

    // Read and check fundamental
    int fundamental = 0;
    int scan_test = sscanf(argv[1], "%d", &fundamental);
    if (scan_test != 1) {
        exit_error_msg("No fundamental integer argument found");
    }
    if (fundamental < 0) {
        exit_error_msg("Fundamental frequency cannot be negative");
    }

    // Read and check max
    int max = 0;
    scan_test = sscanf(argv[2], "%d", &max);
    if (scan_test != 1) {
        exit_error_msg("No max integer argument found");
    }
    if (max < 0) {
        exit_error_msg("Maximum cannot be negative");
    }

    vector<Harmonic> harmonic_series;

    // Create harmonics in descending order
    harmonic_series[0].print_header() << endl;

    for (int factor = max; factor > 0; --factor) {
        harmonic_series.push_back(Harmonic(fundamental, factor));
    }
    for (auto i = harmonic_series.cbegin(); i != harmonic_series.cend(); ++i) {
        (*i).print() << endl;
    }

    return 0;
}

// Exit with an error message
void exit_error_msg(string msg) {
    cerr << msg << endl;
    exit(EXIT_FAILURE);
}

