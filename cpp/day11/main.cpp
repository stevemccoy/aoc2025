//
// main.cpp : Factory
//

#include "common.h"
#include <iostream>
#include <set>
#include <map>
#include <list>

// The connections given in input.
typedef std::map<std::string,std::vector<std::string>> graph_t;

// Populate the graph from the given line.
void process_line(const std::string& line, graph_t& graph) {
    auto sl1 = split_delim(trim(line), ':');
    auto sl2 = split_delim(trim(sl1[1]), ' ');
    for (auto d : sl2) {
        std::cout << "s(" << sl1[0] << "," << d <<")." << std::endl;
    }
}

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    graph_t graph;
    for (auto line : lines) {
        process_line(line, graph);
    }
    return 0;
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    return 0;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 11 - Reactor\n";

    std::cout << "Part 1." << std::endl;
    count = part1("test2.txt");
    std::cout << "Number of paths from you to out in test.txt = " << count << std::endl;

    // count = part1("input.txt");
    // std::cout << "Minimum number of button presses in input.txt = " << count << std::endl;

    // std::cout << "Part 2." << std::endl;
    // count = part2("test.txt");
    // std::cout << "Minimum number of button presses in test.txt = " << count << std::endl;

    // count = part2("input.txt");
    // std::cout << "Minimum number of button presses in input.txt = " << count << std::endl;

    return 0;
}
