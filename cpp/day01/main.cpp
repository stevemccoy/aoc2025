//
// main.cpp : Linen Layout
//

#include "common.h"
#include <stdio.h>
#include <iostream>
#include <cstdint>

const number BIG_NUMBER = 1000000000000L;

int part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    int position = 50;
    int count = 0;
    for (auto line : lines) {
        char direction = line[0];
        int clicks = atoi(line.substr(1).c_str());
        if (direction == 'L') {
            position -= clicks;
            while (position < 0) {
                position += 100;
            }
        }
        else {
            position += clicks;
            while (position > 99) {
                position -= 100;
            }
        }
        if (position == 0) {
            count++;
        }
    }
    return count;
}

int part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    int position = 50;
    int count = 0;
    for (auto line : lines) {
        char direction = line[0];
        int clicks = atoi(line.substr(1).c_str());
        if (clicks > 0) {
            bool start_zero = (position == 0);
            if (direction == 'L') {
                position -= clicks;
                while (position < 0) {
                    position += 100;
                    count++;
                }
                if (start_zero) {
                    count--;
                }
            }
            else if (direction == 'R') {
                position += clicks;
                while (position > 99) {
                    position -= 100;
                    count++;
                }
                if (position == 0) {
                    count--;
                }
            }
            else {
                std::cerr << "Error in line " << line << " - unmatched direction character (not L,R)" << std::endl;
                return -1;
            }
            // If end at zero, count one.
            if (position == 0) {
                count++;
            }
        }
    }
    return count;
}

int main()
{
    int count = 0;
    std::cout << "Advent of Code 2025\nDay 1 - Secret Entrance\n";
    std::cout << "Part 1.\n";
    std::cout << "Password for test.txt = ";
    count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Password for input.txt = ";
    count = part1("input.txt");
    std::cout << count << std::endl;

    std::cout << "Part 2.\n";
    std::cout << "Password for test.txt = ";
    count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Password for input.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
