/* otp.cpp
 * One-Time Pad Encryption
 *
 * Andrew Cashner
 * 2026/02/03
 *
 * Given a message, encrypt it by XOR-ing it with a random string of the same
 * length. Decrypt it the same way.
 */

#include <iostream>
#include <cstdlib>
#include <ctime>
#include <string>
#include <vector>
#include <exception>
#include <limits>
#include <experimental/iterator>

class OTP {
    public:
        OTP(int length);
        std::vector<int> encrypt(std::string& msg);
        std::string decrypt(std::vector<int>& msg);
        static void display_vector(std::vector<int>& vector);

    private:
        int length;
        std::vector<int> key;
        std::vector<int> generate_key(int length);
        std::vector<int> vector_encrypt(std::vector<int>& msg);
        std::vector<int> vector_decrypt(std::vector<int>& msg);
};

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: otp 'MESSAGE'\n";
        exit(EXIT_FAILURE);
    }

    std::string msg { argv[1] };
    OTP otp { (int) msg.length() };

    try 
    {
        std::vector<int> ciphered { otp.encrypt(msg) };
        OTP::display_vector(ciphered);

        std::string deciphered { otp.decrypt(ciphered) };
        std::cout << deciphered << "\n";
    }
    catch (const std::exception& e)
    {
        std::cerr << "OTP error: " << e.what() << "\n";
    }

    return 0;
}

OTP::OTP(int length)
{
    std::srand(std::time(NULL));

    this->length = length;
    this->key = this->generate_key(length);
}

std::vector<int> OTP::generate_key(int length)
{
    std::vector<int> key;

    for (int i = 0; i < length; ++i)
    {
        int code = std::rand() % std::numeric_limits<int>::max();
        key.push_back(code);
    }

    return key;
}

std::vector<int> OTP::vector_encrypt(std::vector<int>& msg)
{
    if (this->key.size() != msg.size())
    {
        throw "Error: Invalid key";
    }

    std::vector<int> cipher;

    for (int i = 0; i < (int) msg.size(); ++i)
    {
        cipher.push_back(msg[i] ^ this->key[i]);
    }

    return cipher;
}

std::vector<int> OTP::vector_decrypt(std::vector<int>& msg)
{
    if (this->key.size() != msg.size())
    {
        throw "Error: Invalid key";
    }

    std::vector<int> decipher;

    for (int i = 0; i < (int) msg.size(); ++i)
    {
        decipher.push_back(msg[i] ^ key[i]);
    }

    return decipher;
}
        
std::vector<int> OTP::encrypt(std::string& msg)
{
    std::vector<int> msg_bits;

    for (auto c : msg)
    {
        msg_bits.push_back((int) c);
    }

    return this->vector_encrypt(msg_bits);
}

std::string OTP::decrypt(std::vector<int>& cipher)
{

    std::vector<int> deciphered { this->vector_decrypt(cipher) };

    std::string msg { "" };

    for (auto n : deciphered)
    {
        msg += (char) n;
    }

    return msg;
}

void OTP::display_vector(std::vector<int>& vector)
{
    std::copy(std::begin(vector), 
              std::end(vector),
              std::experimental::make_ostream_joiner(std::cout, " "));

    std::cout << "\n";
}
