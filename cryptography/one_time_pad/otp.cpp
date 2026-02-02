/* otp.cpp
 * One-Time Pad Encryption
 *
 * Andrew Cashner
 * 2026/02/02
 *
 * Given a message, encrypt it by XOR-ing it with a random string of the same
 * length. Decrypt it the same way.
 */

#include <iostream>
#include <cstdlib>
#include <ctime>
#include <string>
#include <vector>

std::string generate_key(std::string msg);
std::vector<int> encrypt(std::string msg, std::string key);
std::string decrypt(std::vector<int> msg, std::string key);

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: otp 'MESSAGE'\n";
        exit(EXIT_FAILURE);
    }

    std::srand(std::time(NULL));

    std::string msg { argv[1] };
    std::string key { generate_key(msg) };
    
    std::cout << msg << "\n";
    std::cout << key << "\n";

    std::vector<int> cipher { encrypt(msg, key) };

    for (auto n : cipher)
    {
        std::cout << n << " ";
    }
    std::cout << "\n";

    std::string plaintext { decrypt(cipher, key) };
    std::cout << plaintext << "\n";

    return 0;
}


std::string generate_key(std::string msg)
{
    std::string key { "" };

    for (int i = 0; msg[i] != '\0'; ++i)
    {
        int key_char = std::rand() % ('z' - 'A');
        key += 'A' + key_char;
    }

    return key;
}

std::vector<int> encrypt(std::string msg, std::string key)
{
    std::vector<int> cipher;

    for (int i = 0; msg[i] != '\0'; ++i)
    {
        cipher.push_back(msg[i] ^ key[i]);
    }

    return cipher;
}

std::string decrypt(std::vector<int> msg, std::string key)
{
    std::string plaintext { "" };

    for (int i = 0; i < (int) msg.size() && key[i] != '\0'; ++i)
    {
        int plain_char { msg[i] ^ key[i] };
        plaintext += (char)plain_char;
    }

    return plaintext;
}
