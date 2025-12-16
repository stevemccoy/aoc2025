//
// main.cpp : Laboratories
//

#include "common.h"
#include <iostream>
#include <set>
#include <map>

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0;
    std::string line;
    std::set<size_t> beams, changes;
    std::string s;
    // Find single starting position.
    line = trim(lines[0]);
    size_t nc = line.size();

    beams.insert(line.find('S'));
    for (auto rawline : lines) {
        line = trim(rawline);
        changes.clear();

        for (auto b1 : beams) {
            if (line[b1] == '^') {
                changes.insert(b1);
                count++;
            }
        }
        for (auto b2 : changes) {
            beams.erase(b2);
            if (b2 < nc - 1) {
                beams.insert(b2 + 1);
            }
            if (b2 > 0) {
                beams.insert(b2 - 1);
            }
        }
    }
    return count;
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    std::string line;
    std::map<number, number> beams, changes;
    std::string s;
    // Find single starting position.
    line = trim(lines[0]);
    size_t nc = line.size();

    beams[line.find('S')] = 1L;
    for (auto rawline : lines) {
        line = trim(rawline);
        changes.clear();

        for (auto b1 : beams) {
            if (line[b1.first] == '^') {
                changes[b1.first] = b1.second;
            }
        }
        for (auto b2 : changes) {
            beams[b2.first] -= b2.second;
            if (b2.first < nc - 1) {
                size_t pos = b2.first + 1;
                if (beams.find(pos) == beams.end()) {
                    beams[pos] = b2.second;
                }
                else {
                    beams[pos] += b2.second;
                }
            }
            if (b2.first > 0) {
                size_t pos = b2.first - 1;
                if (beams.find(pos) == beams.end()) {
                    beams[pos] = b2.second;
                }
                else {
                    beams[pos] += b2.second;
                }
            }
        }
    }
    number count = 0L;
    for (auto p1 : beams) {
        count += p1.second;
    }
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 7 - Laboratories\n";

    std::cout << "Part 1." << std::endl;
    std::cout << "Beam splits in test.txt = ";
    count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Beam splits in input.txt = ";
    count = part1("input.txt");
    std::cout << count << std::endl;

    std::cout << "Part 2." << std::endl;
    std::cout << "Number of timelines in test.txt = ";
    count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Number of timelines in input.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
