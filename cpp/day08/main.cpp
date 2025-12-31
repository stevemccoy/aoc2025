//
// main.cpp : Playground
//

#include "common.h"
#include "Coord.h"

#include <iostream>
#include <set>
#include <map>
#include <algorithm>

Coord process_line(const std::string& line) {
    auto term_strings = split_delim(trim(line), ',');
    if (term_strings.size() != 3) {
        std::cerr << "ERROR: Bad Coord line: " << line << std::endl;
        exit(1);
    }
    Coord c(atoll(term_strings[0].c_str()), atoll(term_strings[1].c_str()), atoll(term_strings[2].c_str()));
    return c;
}

typedef struct d_entry { size_t i; size_t j; number d; } distance_row;

std::vector<std::set<size_t>>::iterator find_group_for(size_t index, std::vector<std::set<size_t>>& all_groups) {
    for (auto it = all_groups.begin(); it != all_groups.end(); it++) {
        if (it->contains(index)) {
            return it;
        }
    }
    return all_groups.end();
}

number part1(const char* file_name, size_t num_merges) {
    std::vector<std::string> lines = read_input_file(file_name);
    std::string line;
    std::vector<std::string> term_strings;
    std::map<size_t, Coord> boxes;
    std::vector<distance_row> distances;
    number count = 0L, d = 0L, ceiling = __LONG_LONG_MAX__;
    size_t index = 0;
    // Build limited table of distances between junction boxes.
    for (auto rawline : lines) {
        Coord c1 = process_line(rawline);
        boxes[index] = c1;
        for (auto other : boxes) {
            if (other.first == index) {
                continue;
            }
            d = other.second.distance(c1);
            if (d > ceiling) {
                continue;
            }
            distance_row dr1 = {other.first, index, d};
            // Insert this distance entry in the table if it is low enough.
            size_t i = 0;
            bool f_append = true;
            for (auto dr2 : distances) {
                if (dr2.d > d) {
                    distances.emplace(distances.begin() + i, dr1);
                    f_append = false;
                    break;
                }
                if (i > num_merges) {
                    ceiling = dr2.d;
                    f_append = false;
                    break;
                }
                i++;
            }
            if (f_append) {
                distances.emplace_back(dr1);
            }
        }
        index++;
    }
    // Now group individual boxes together.
    std::vector<std::set<size_t>> groups;
    for (int di = 0; di < num_merges; di++) {
        auto dr3 = distances[di];
        auto j1 = dr3.i;
        auto j2 = dr3.j;
        std::vector<std::set<size_t>>::iterator gi1, gi2;
        std::set<size_t> g3;

        gi1 = find_group_for(j1, groups);
        gi2 = find_group_for(j2, groups);

        if (gi1 == groups.end()) {
            if (gi2 == groups.end()) {
                g3.clear();
                g3.insert(j1);
                g3.insert(j2);
                groups.emplace_back(g3);
            }
            else {
                gi2->insert(j1);
            }
        }
        else {
            if (gi2 == groups.end()) {
                gi1->insert(j2);
            }
            else {
                if (gi1 != gi2) {
                    g3.clear();
                    g3.merge(*gi1);
                    g3.merge(*gi2);
                    if (gi1 > gi2) {
                        groups.erase(gi1);
                        groups.erase(gi2);
                    }
                    else {
                        groups.erase(gi2);
                        groups.erase(gi1);
                    }
                    groups.emplace_back(g3);
                }
            }
        }
    }
    // Calculate result based on 3 most populous groups.
    std::vector<size_t> sizes;
    for (auto g4 : groups) {
        size_t s1 = g4.size();
        size_t i = 0;
        bool f_append = true;
        for (auto s2 : sizes) {
            if (s1 > s2) {
                sizes.insert(sizes.begin() + i, s1);
                f_append = false;
                break;
            }
            if (i > 2) {
                f_append = false;
                break;
            }
            i++;
        }
        if (f_append) {
            sizes.push_back(s1);
        }
    }
    count = sizes[0] * sizes[1] * sizes[2];
    return count;
}

number part2(const char* file_name) {
    std::map<size_t, Coord> boxes;
    std::vector<distance_row> distances;
    size_t index = 0;
    // Read coordinates.
    for (auto rawline : read_input_file(file_name)) {
        Coord c1 = process_line(rawline);
        boxes[index++] = c1;
    }
    // Populate distances - for each distinct pair of boxes.
    distance_row dr1;
    number d;
    for (auto pi1 = boxes.begin(); pi1 != boxes.end(); pi1++) {
        dr1.i = pi1->first;
        auto pi2 = pi1;
        for (pi2++; pi2 != boxes.end(); pi2++) {
            dr1.j = pi2->first;
            dr1.d = d = pi2->second.distance(pi1->second);
            // Insert record into distances table to maintain in sorted order of distance.
            size_t i = 0;
            bool f_append = true;
            for (auto dr2 : distances) {
                if (dr2.d > d) {
                    distances.emplace(distances.begin() + i, dr1);
                    f_append = false;
                    break;
                }
                i++;
            }
            if (f_append) {
                distances.emplace_back(dr1);
            }
        }
    }
    // Print out shortest connections:
    // distance_row dr3;
    // for (int i = 0; i < num_merges; i++) {
    //     dr3 = distances[i];
    //     Coord c1 = boxes[dr3.i];
    //     Coord c2 = boxes[dr3.j];
    //     std::cout << "From " << dr3.i << " " << c1.display() << " to " << dr3.j << " " << c2.display() << " distance " << dr3.d << std::endl;
    // }

    // Now group individual boxes together.
    std::map<size_t,size_t> box_groups;
    for (auto b1 : boxes) {
        box_groups[b1.first] =  0;
    }
    size_t count_assigned = 0, num_boxes = boxes.size();
    size_t group_id = 1, num_groups = 0;
    number last_x1 = 0, last_x2 = 0;
    for (int di = 0; (count_assigned < num_boxes) || (num_groups != 1); di++) {
        auto dr3 = distances[di];
        auto j1 = dr3.i;
        auto j2 = dr3.j;
        auto bg1 = box_groups[j1];
        auto bg2 = box_groups[j2];
        last_x1 = boxes[j1].x;
        last_x2 = boxes[j2].x;
        if (bg1 == 0) {
            if (bg2 == 0) {
                box_groups[j1] = box_groups[j2] = group_id;
                group_id++;
                num_groups++;
                count_assigned += 2;
            }
            else {
                box_groups[j1] = bg2;
                count_assigned++;
            }
        }
        else {
            if (bg2 == 0) {
                box_groups[j2] = bg1;
                count_assigned++;
            }
            else {
                if (bg1 != bg2) {
                    // Merge group bg1 into bg2.
                    std::set<size_t> changes;
                    for (auto p1 : box_groups) {
                        if (p1.second == bg1) {
                            changes.insert(p1.first);
                        }
                    }
                    for (auto id : changes) {
                        box_groups[id] = bg2;
                    }
                    num_groups--;
                }
            }
        }
    }
    // Calculate result based on X coordinates of the last 2 connected boxes.
    return last_x1 * last_x2;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 8 - Playground\n";

    std::cout << "Part 1." << std::endl;
    count = part1("test.txt", 10);
    std::cout << "Product in test.txt = ";
    std::cout << count << std::endl;

    // 56832 too low.
    count = part1("input.txt", 1000);
    std::cout << "Product in input.txt = ";
    std::cout << count << std::endl;

    std::cout << "Part 2." << std::endl;
    count = part2("test.txt");
    std::cout << "Product in test.txt = ";
    std::cout << count << std::endl;

    count = part2("input.txt");
    std::cout << "Product in input.txt = ";
    std::cout << count << std::endl;

    return 0;
}
