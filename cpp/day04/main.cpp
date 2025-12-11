//
// main.cpp : Printing Department
//

#include "common.h"
#include "Grid.h"
#include <stdio.h>
#include <iostream>
#include <cstdint>
#include <cstring>
#include <map>
#include <set>

std::vector<Position> removables(Grid& grid) {
    std::vector<Position> result;
    Position p;
    for (int row = 0; row < grid.num_rows; row++) {
        p.row = row;
        for (int col = 0; col < grid.num_cols; col++) {
            p.col = col;
            if (grid.get(col,row) == '@') {
                if (grid.count_neighbours_with(p, '@') < 4) {
                    result.push_back(p);
                }
            }
        }
    }
    return result;
}

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    Grid grid;
    grid.setup(lines);
    auto rl = removables(grid);
    return rl.size();
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    Grid grid;
    grid.setup(lines);
    number count = 0L;
    auto rl = removables(grid);
    count = rl.size();
    while (rl.size() > 0) {
        for (auto p : rl) {
            grid.set(p.col, p.row, 'x');
        }
        rl = removables(grid);
        count += rl.size();
    }
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 4 - Printing Department\n";

    std::cout << "Part 1." << std::endl;
    std::cout << "Number of accessible rolls of paper in test.txt = ";
    count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Number of accessible rolls of paper in input.txt = ";
    count = part1("input.txt");
    std::cout << count << std::endl;

    std::cout << "Part 2." << std::endl;
    std::cout << "Total number of rolls of paper removed in test.txt = ";
    count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Total number of rolls of paper removed in input.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
