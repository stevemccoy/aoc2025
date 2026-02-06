//
// main.cpp : Movie Theater
//

#include "common.h"
#include "Coord.h"
#include <iostream>
#include <set>
#include <map>

Coord process_line(const std::string& line) {
    auto term_strings = split_delim(trim(line), ',');
    if (term_strings.size() != 2) {
        std::cerr << "ERROR: Bad Coord line: " << line << std::endl;
        exit(1);
    }
    Coord c(atoll(term_strings[0].c_str()), atoll(term_strings[1].c_str()));
    return c;
}

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0;
    std::vector<Coord> tiles;
    for (auto rawline : lines) {
        tiles.push_back(process_line(rawline));
    }
    number area = 0, max_area = 0;
    for (auto i = tiles.begin(); i != tiles.end(); i++) {
        auto j = i;
        for (j++; j != tiles.end(); j++) {
            area = i->rect_area(*j);
            if (area > max_area) {
                max_area = area;
            }
        }       
    }
    return max_area;
}

// 0 = no crossings
// 1 = simple line crossings.
// 2 = shared portion of edge.
int classify_crossings(
    const std::pair<Coord,Coord>& line,
    const std::vector<std::pair<Coord,Coord>>& edges) {

    for (auto e : edges) {
	
    }
}


number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    std::string line;
    std::map<number, number> beams, changes;
    std::string s;
    number count = 0L;
    std::vector<Coord> tiles;
    for (auto rawline : lines) {
        tiles.push_back(process_line(rawline));
    }

    std::vector<std::pair<Coord,Coord>> edges;
    std::vector<std::pair<Coord,Coord>> patches;

    std::pair<Coord,Coord> p, e1, e2;
    for (int i = 1; i < tiles.size(); i++) {
	p.first = tiles[i - 1];
	p.second = tiles[i];
	edges.emplace_back(p);
    }
    p.first = p.second;
    p.second = tiles[0];
    edges.emplace_back(p);

    for (int i = 1; i < edges.size(); i++) {
	e1 = edges[i - 1];
	e2 = edges[i];

	e1.first 
    }
    
/*
    Trace red tiles round circuit
*/
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 9 - Movie Theater\n";

    std::cout << "Part 1." << std::endl;
    std::cout << "Max area in test.txt = ";
    count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Max area in input.txt = ";
    count = part1("input.txt");
    std::cout << count << std::endl;

    // std::cout << "Part 2." << std::endl;
    // std::cout << "Number of timelines in test.txt = ";
    // count = part2("test.txt");
    // std::cout << count << std::endl;

    // std::cout << "Number of timelines in input.txt = ";
    // count = part2("input.txt");
    // std::cout << count << std::endl;

    return 0;
}
