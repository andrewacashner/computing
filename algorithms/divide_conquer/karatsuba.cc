/* Karatsuba multiplication of long integers
 * Andrew Cashner
 * 2025/11/10
 * From class notes and https://en.wikipedia.org/wiki/Karatsuba_algorithm
 */

#include <iostream>
#include <string>
#include <cstdlib>
#include <print>
#include <cmath>

const int BASE = 10;

long long karatsuba_multiply(long long num_A, long long num_B);

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        std::cout << 
            "Usage: karatsuba NUM1 NUM2 (to calculate NUM1 * NUM2)\n";
        std::exit(EXIT_FAILURE);
    }

    long long num_A { std::stoi(argv[1]) };
    long long num_B { std::stoi(argv[2]) };

    long long product { karatsuba_multiply(num_A, num_B) };

    std::print("{} x {} = {}\n", num_A, num_B, product);
//    std::cout << num_A << " x " << num_B << " = " << product << "\n";

    return 0;
}

int digits(long long n)
{
    return std::to_string(n).size();
}

std::pair<long long, long long> split_at(long long n, int digits)
{
    long long high { n / (long long) std::pow(BASE, digits) };
    long long low { n % (long long) std::pow(BASE, digits) };
    return std::pair(high, low);
}

long long karatsuba_multiply(long long in1, long long in2)
{
    if (in1 < BASE || in2 < BASE)
    {
        return in1 * in2;
    }

    int most_digits { std::max(digits(in1), digits(in2)) };
    int mid { (int) std::floor((double) most_digits / 2.0) };

    std::pair<int, int> in1_split { split_at(in1, mid) };
    std::pair<int, int> in2_split { split_at(in2, mid) };

    long long A { in1_split.first };
    long long B { in1_split.second };
    long long C { in2_split.first };
    long long D { in2_split.second };

    long long X { karatsuba_multiply(A, C) };
    long long Y { karatsuba_multiply(B, D) };
    long long Z { karatsuba_multiply(A + B, C + D) };

    return X * std::pow(BASE, mid * 2) 
           + (Z - X - Y) * std::pow(BASE, mid) 
           + Y;
}
