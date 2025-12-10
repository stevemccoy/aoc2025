//
// main.cpp : Lobby
//

#include "common.h"
#include <iostream>

// Choose 2 batteries in each bank for maximum joltage.
number part1(const char* file_name) {
    char buffer[20];
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0L;
    std::vector<number> invalid_ids;
    for (auto line : lines) {
        line = trim(line);
        int i1 = max_char_in_line(line.substr(0, line.length() - 1));
        int d1 = line[i1] - '0';
        int i2 = max_char_in_line(line.substr(i1 + 1));
        int d2 = line[i1 + 1 + i2] - '0';
        int joltage = d1 * 10 + d2;
        count += joltage;
    }
    return count;
}

number max_joltage_choose_n_from(const std::string& bank, int n, size_t starting_at) {
    number jolts = 0L;
    for (int i = 0; i < n; i++) {
        size_t i1 = max_char_in_line(bank.substr(starting_at, bank.length() - starting_at - n + i + 1));
        int d1 = bank[starting_at + i1] - '0';
        starting_at += (i1 + 1);
        jolts = jolts * 10 + d1;
    }
    return jolts;
}

// Choose 12 batteries in each bank for maximum joltage.
number part2(const char* file_name) {
    char buffer[20];
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0L;
    std::vector<number> invalid_ids;
    for (auto line : lines) {
        line = trim(line);
        number joltage = max_joltage_choose_n_from(line, 12, 0);
        count += joltage;
    }
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 3 - Lobby\n";
    std::cout << "Part 1." << std::endl;
    std::cout << "Sum of joltages for banks in test.txt = ";
    // count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Sum of joltages for banks in input.txt = ";
    // count = part1("input.txt");
    std::cout << count << std::endl;

    std::cout << "Part 2." << std::endl;
    std::cout << "Sum of joltages for banks in test.txt = ";
    count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Sum of joltages for banks in test.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
