//
// main.cpp : Trash Compactor
//

#include "common.h"
#include <iostream>

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0;
    std::string line;
    std::vector<std::vector<number>> working;
    std::string s;
    for (auto rawline : lines) {
        line = trim(rawline);
        std::vector<number> values;
        auto terms = split_delim(trim(line), ' ');
        // Check if we've finished reading numbers.
        char ch = trim(terms[0])[0];
        if (!isdigit(ch)) {
            trim_remove_empties(terms);
            for (int i = 0; i < terms.size(); i++) {
                ch = terms[i][0];
                number result = 0L;
                if (ch == '*') {
                    result = 1L;
                    for (int j = 0; j < working.size(); j++) {
                        result *= working[j][i];
                    }
                }
                else {
                    for (int j = 0; j < working.size(); j++) {
                        result += working[j][i];
                    }
                }
                count += result;
            }
            break;
        }
        // Numbers - store them in the working buffer.
        for (auto term : terms) {
            s = trim(term);
            if (s.size() > 0) {
                values.push_back(atoll(s.c_str()));
            }
        }
        working.push_back(values);
    }
    return count;
}

number calc_subtotal(char op, std::vector<number> &buffer)
{
    number result;
    if (op == '*') {
        result = 1L;
        for (int j = 0; j < buffer.size(); j++) {
            result *= buffer[j];
        }
    }
    else {
        result = 0L;
        for (int j = 0; j < buffer.size(); j++) {
            result += buffer[j];
        }
    }
    return result;
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0;
    std::string s;

    size_t num_cols = lines[0].size();
    size_t num_rows = lines.size();
    std::vector<number> buffer;
    number result;
    char op = ' ';
    for (int c = num_cols - 1; c >= 0; c--) {
        s.clear();
        for (size_t r = 0; r < num_rows - 1; r++) {
            s += lines[r][c];
        }
        s = trim(s);
        if (s.size() == 0) {
            result = calc_subtotal(op, buffer);
            count += result;
            buffer.clear();
        }
        else {
            op = lines[num_rows - 1][c];
            buffer.push_back(atoll(s.c_str()));
        }
    }
    count += calc_subtotal(op, buffer);
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 6 - Trash Compactor\n";

    std::cout << "Part 1." << std::endl;
    std::cout << "Grand total in test.txt = ";
    count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Grand total in input.txt = ";
    count = part1("input.txt");
    std::cout << count << std::endl;

    std::cout << "Part 2." << std::endl;
    std::cout << "Grand total in test.txt = ";
    count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Grand total in input.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
