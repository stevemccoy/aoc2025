//
// main.cpp : Gift Shop
//

#include "common.h"
#include <iostream>
#include <map>
#include <set>

const char* pow10_string_lookup[] = {"1", "10", "100", "1000", "10000", "100000", "1000000", "10000000", "100000000", 
    "1000000000", "10000000000", "100000000000", "1000000000000", "10000000000000", "100000000000000", "1000000000000000"};
const number pow10_number_lookup[] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000, 
    100000000000, 1000000000000, 10000000000000, 100000000000000, 1000000000000000 };

// Factors apart from unity.
static const std::map<int, std::vector<int>> factors = {{1, {}}, {2, {2}}, {3, {3}}, {4, {2,4}}, {5, {5}}, {6, {2,3,6}},
 {7, {7}}, {8, {2,4,8}}, {9, {3,9}}, {10, {2,5,10}}, {11, {11}}, {12, {2,3,4,6,12}},
 {13, {13}}, {14, {2,7,14}}, {15, {3,5,15}}, {16, {2, 4, 8, 16}}};

number part1(const char* file_name) {
    char buffer[20];
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0L;
    std::vector<number> invalid_ids;

    for (auto line : lines) {
        for (auto range_string1 : split_delim(line, ',')) {
            auto range_vec1 = split_delim(range_string1, '-');
            size_t num_digits_start = range_vec1[0].length();
            bool nds_odd = (num_digits_start & 1);
            // No invalid IDs possible if whole range has odd number of digits.
            if ((num_digits_start == range_vec1[1].length())&&(nds_odd)) {
                continue;
            }
            std::string half_string = (nds_odd ? pow10_string_lookup[num_digits_start / 2] : range_vec1[0].substr(0, num_digits_start / 2));
            number half_value = atoi(half_string.c_str());
            number half_ceiling = pow10_number_lookup[half_string.length()];

            number start = atoi(range_vec1[0].c_str());
            number finish = atoi(range_vec1[1].c_str());

            number n = 0L;
            for (number h = half_value; n <= finish; h++) {
                if (h >= half_ceiling) {
                    half_ceiling *= 10L;
                }
                n = (half_ceiling * h) + h;
                if ((n >= start) && (n <= finish)) {
                    invalid_ids.push_back(n);
                    count += n;
                }
            }
        }
    }
    // 4228109920 too low.
    return count;
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0L;
    std::set<number> invalid_ids;

    for (auto line : lines) {
        auto range_stanzas = split_delim(line, ',');
        for (auto range_string1 : range_stanzas) {
            auto range_vec1 = split_delim(range_string1, '-');
            number start = atoi(range_vec1[0].c_str());
            number finish = atoi(range_vec1[1].c_str());

            size_t num_digits = range_vec1[0].length();
            size_t num_digits_start = num_digits;
            while (num_digits <= range_vec1[1].length()) {
                if (!factors.contains(num_digits)) {
                    std::cerr << "Error: Line contains start number too big! " << range_string1 << std::endl;
                    exit(1);
                }
                auto factors_list = factors.at(num_digits);
                for (auto num_divs : factors_list) {
                    size_t size_div = num_digits / num_divs;

                    std::string div_string = (num_digits > num_digits_start) ? pow10_string_lookup[size_div - 1] : range_vec1[0].substr(0, size_div);
                    number div_value = atoi(div_string.c_str());
                    number div_ceiling = pow10_number_lookup[div_string.length()];

                    number n = 0L;
                    for (number h = div_value; n <= finish; h++) {
                        if (h >= div_ceiling) {
                            div_ceiling *= 10L;
                        }
                        n = h;
                        for (int i = 1; i < num_divs; i++) {
                            n = (n * div_ceiling) + h;
                        }
                        if ((n >= start)&&(n <= finish)) {
                            if (!invalid_ids.contains(n)) {
                                invalid_ids.insert(n);
                                count += n;
                            }
                        }
                    }
                }
                num_digits++;
            }
        }
    }
    return count;       // 4228109920 too low.
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 2 - Gift Shop\n";
    std::cout << "Part 1 - solved using Prolog. See elsewhere.\n";
    std::cout << "Part 2." << std::endl;
    std::cout << "Sum of invalid IDs for test.txt = ";
    // count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Sum of invalid IDs for input.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
