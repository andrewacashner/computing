/* Count Inversions
 *
 * Andrew Cashner
 * 2025/12/11
 */

#include <cassert>
#include <iostream>
#include <format>
#include <vector>
#include <string>
#include <algorithm>

class InversionCount
{
    private:
        int count { 0 };
        std::vector<int> nums {};

    public:
        InversionCount() = default; 

        InversionCount(std::vector<int> in_nums):
            count { 0 }, nums { in_nums } {};

        void sort_count();

        InversionCount merge_count(InversionCount other);

        int get_count() { return count; }

        int size() { return nums.size(); }

        std::string to_string();
};

int count_inversions(std::vector<int> nums);

int main(void)
{
    typedef std::pair<std::vector<int>, int> TestCase;

    std::vector<TestCase> input { 
        { { 5, 2, 0, 9, 6, 12 },   4 },
        { { 2, 4, 7, 0, 6, 9, 5 }, 7 },
    };

    std::vector<bool> test_results {};

    for (auto& test_case : input)
    {
        int count = count_inversions(test_case.first);
        bool result = (count == test_case.second);
        test_results.push_back(result);
        std::cout << (result ? "PASS" : "FAIL") << "\n";
    }

    bool passed = (std::all_of(test_results.begin(),
                                test_results.end(),
                                [](bool result) { return result; }));
    std::cout << (passed ? "All passed" : "Some failed") << "\n";

    return 0;
}

InversionCount InversionCount::merge_count(InversionCount other)
{
    InversionCount merged {};

    int this_pos { 0 };
    int other_pos { 0 };
    
    while (this_pos < this->size() && other_pos < other.size())
    {
        int next;
        if (this->nums[this_pos] < other.nums[other_pos])
        {
            next = this->nums[this_pos];
            ++this_pos;
        }
        else
        {
            next = other.nums[other_pos];
            ++other_pos;

            // Add the remaining length of the left (this) array
            // because every subsequent value will have an inversion relative
            // to this value
            merged.count += this->size() - this_pos;
        }

        merged.nums.push_back(next);
    }

    while (this_pos < this->size())
    {
        merged.nums.push_back(this->nums[this_pos]);
        ++this_pos;
    }

    while (other_pos < other.size())
    {
        merged.nums.push_back(other.nums[other_pos]);
        ++other_pos;
    }

    return merged;
}

void InversionCount::sort_count()
{
    InversionCount inversions { nums };

    int len = nums.size();

    if (len > 1) 
    { 
        int midpoint = len / 2;
        auto left_nums = std::vector<int>(nums.begin(), 
                                          nums.begin() + midpoint);
        auto right_nums = std::vector<int>(nums.begin() + midpoint, 
                                           nums.end());

        InversionCount left { left_nums };
        left.sort_count();

        InversionCount right { right_nums };
        right.sort_count();

        InversionCount merged { left.merge_count(right) };
        merged.count += left.count + right.count;

        this->count = merged.count;
        this->nums = merged.nums;
    }
}

std::string InversionCount::to_string()
{
    std::string list {"["};
    for (auto position = nums.begin(); position != nums.end(); ++position)
    {
        list += std::to_string(*position);
        if (position < nums.end() - 1)
        {
            list += ", ";
        }
    }
    list += "]";

    return std::format("{{ count: {}, nums: {} }}", count, list);
}

int count_inversions(std::vector<int> nums)
{

    InversionCount inversions { nums };
    inversions.sort_count();
    return inversions.get_count();
}
