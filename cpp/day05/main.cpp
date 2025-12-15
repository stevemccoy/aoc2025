//
// main.cpp : Cafeteria
//

#include "common.h"
#include <iostream>

bool is_fresh(number ingredient, const std::vector<std::pair<number,number>>& fresh_ranges) {
    for (auto p : fresh_ranges) {
        if (ingredient >= p.first && ingredient <= p.second) {
            return true;
        }
    }
    return false;
}

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0;
    std::vector<std::pair<number,number>> fresh_ranges;
    bool fresh_done = false;

    for (auto line : lines) {
        auto tline = trim(line);
        if (fresh_done) {
            if (is_fresh(atoll(tline.c_str()), fresh_ranges)) {
                count++;
            }
        }
        else if (tline == "") {
            fresh_done = true;
        }
        else {
            auto pair_vec = split_delim(tline, '-');
            fresh_ranges.push_back(std::pair<number,number>(atoll(pair_vec[0].c_str()), atoll(pair_vec[1].c_str())));
        }
    }
    return count;
}

/*
    Overlap cases:

    0 = unknown
    1 = non-overlapping, a entirely below b (la,ha < lb,hb)
    2 = partial overlap, a lower (la,lb,ha,hb)
    3 = containment, a contains b (la,lb,hb,ha)
    4 = partial overlap, b lower (lb,la,hb,ha)
    5 = non-overlap, b entirely below a (lb,hb < la,ha)
    6 = containment, b contains a (lb,la,ha,hb)
*/

int classify_overlap(number low_a, number high_a, number low_b, number high_b) {
    if (high_a < low_b) {
        return 1;   
    }
    else if (high_b < low_a) {
        return 5;
    }
    else if ((high_b <= high_a)&&(low_a <= low_b)) {
        return 3;
    }
    else if ((high_a <= high_b)&&(low_b <= low_a)) {
        return 6;
    }
    else if ((high_a < high_b)&&(low_a < low_b)) {
        return 2;
    }
    else if ((high_b < high_a)&&(low_b < low_a)) {
        return 4;
    }
    return 0;
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    number count = 0;
    std::vector<std::pair<number,number>> fresh_ranges;

    for (auto line : lines) {
        auto tline = trim(line);
        if (tline == "") {
            break;
        }
        else {
            auto pair_vec = split_delim(tline, '-');
            number low = atoll(pair_vec[0].c_str());
            number high = atoll(pair_vec[1].c_str());
            bool keep = true;
            number prev_range = 0L;
            for (auto pi = fresh_ranges.begin(); keep && pi != fresh_ranges.end(); pi++) {
                auto ot = classify_overlap(low, high, pi->first, pi->second);
                switch (ot) {
                    case 1: 
                    case 5: break;                  // Non-overlapping
                    case 2: high = pi->first - 1;     // Overlapping
                            break;                  
                    case 4: low = pi->second + 1;     // Overlapping
                            break;
                    case 6: keep = false;           // Containment - prev range contains this.
                            break;
                    case 3: // Containment - new range contains prev range.
                            prev_range = (pi->second - pi->first + 1);
                            pi->first = low;        
                            pi->second = high;
                            // Adjust count for the in place change.
                            count += (high - low + 1 - prev_range);
                            keep = false;
                            break;
                    case 0:
                    default:
                            std::cerr << "Error - unable to classify interval overlap." << std::endl;
                            exit(1);
                            break;
                }
                if (!keep) {
                    break;
                }
            }
            if (keep) {
                count += (high - low + 1);
                fresh_ranges.push_back(std::pair<number,number>(low, high));
            }
        }
    }
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 5 - Cafeteria\n";

    std::cout << "Part 1." << std::endl;
    std::cout << "Number of available fresh ingredients in test.txt = ";
    count = part1("test.txt");
    std::cout << count << std::endl;

    std::cout << "Number of available fresh ingredients in input.txt = ";
    count = part1("input.txt");
    std::cout << count << std::endl;

    std::cout << "Part 2." << std::endl;
    std::cout << "Number of available fresh ingredients in test.txt = ";
    count = part2("test.txt");
    std::cout << count << std::endl;

    std::cout << "Number of available fresh ingredients in input.txt = ";
    count = part2("input.txt");
    std::cout << count << std::endl;

    return 0;
}
