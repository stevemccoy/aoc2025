//
// main.cpp : Factory
//

#include "common.h"
#include <iostream>
#include <set>
#include <map>
#include <list>

// Part 1 search node structure.
typedef struct node_t { std::set<size_t> action; std::set<size_t> state; size_t score; } node;

// Part 2 search node structure.
typedef struct node2_t { std::map<size_t,size_t> actions; std::map<size_t,size_t> joltage; size_t num_actions; size_t score; } node2;

std::set<size_t> apply_button(const std::set<size_t>& button, const std::set<size_t>& state) {
    std::set<size_t> new_state = state;
    for (auto b : button) {
        if (new_state.contains(b)) {
            new_state.erase(b);
        }
        else {
            new_state.insert(b);
        }
    }
    return new_state;
}

// Score the given current state by comparison with required state, scoring 1 for each mismatch.
// Minimum (best) score is 0.
size_t score(const std::set<size_t>& current, const std::set<size_t>& required, size_t num_indicators) {
    size_t score = 0;
    for (int i = 0; i < num_indicators; i++) {
        auto rc = required.contains(i);
        auto cc = current.contains(i);
        if (rc ^ cc) {
            score++;
        }
    }
    return score;
}

std::set<size_t> best_buttons1(const std::string& state, const std::vector<std::set<size_t>>& buttons) {
    // Required indicators to be set.
    size_t num_indicators = state.size();
    std::set<size_t> required;
    for (int i = 0; i < num_indicators; i++) {
        if (state[i] == '#') {
            required.insert(i);
        }
    }
    // Populate first level open list with single button presses.
    std::list<node> open_list;
    std::set<size_t> empty;
    for (size_t bi = 0; bi < buttons.size(); bi++) {
        std::set<size_t> b = buttons[bi];
        size_t s = score(b, required, num_indicators);

        node n1;
        n1.action.insert(bi);
        n1.state = b;
        n1.score = s;

        bool f_append = true;
        for (auto oi = open_list.begin(); oi != open_list.end(); oi++) {
            if (s < oi->score) {
                open_list.emplace(oi, n1);
                f_append = false;
                break;
            }
        }
        if (f_append) {
            open_list.emplace_back(n1);
        }
    }
    // Search till we find perfect match.
    std::set<size_t> result;
    std::set<std::set<size_t>> closed_list;
    while (!open_list.empty()) {
        auto current = open_list.front();
        open_list.pop_front();
        // Check to see if done.
        if (current.score == 0) {
            result = current.action;
            break;
        }
        // Make sure this node has not been expanded already.
        if (closed_list.contains(current.action)) {
            continue;
        }
        closed_list.emplace(current.action);
        // Expand search from current.
        for (size_t bi = 0; bi < buttons.size(); bi++) {
            if (!current.action.contains(bi)) {
                auto state2 = apply_button(buttons[bi], current.state);
                auto score2 = score(state2, required, num_indicators);

                node n2;
                n2.action = current.action;
                n2.action.insert(bi);
                n2.state = state2;
                n2.score = score2;
                size_t num_actions = n2.action.size();

                bool f_append = true;
                for (auto oi = open_list.begin(); oi != open_list.end(); oi++) {
                    if ((num_actions <= oi->action.size()) && (score2 < oi->score)) {
                        open_list.emplace(oi, n2);
                        f_append = false;
                        break;
                    }
                }
                if (f_append) {
                    open_list.emplace_back(n2);
                }
            }
        }
    }
    if (result.size() == 0) {
        std::cerr << "Unable to find buttons for state: " << state << std::endl;
    }
    return result;
}

std::map<size_t,size_t> apply_button2(const std::set<size_t>& button, const std::map<size_t,size_t>& state, size_t num_times) {
    std::map<size_t,size_t> new_state = state;
    for (auto b : button) {
        new_state[b] += num_times;
    }
    return new_state;
}

// Score the given current state by comparison with required state, scoring based on distance of joltages under the required level.
// Any overshoots => cannot be recovered, so return -1.
int score2(const std::map<size_t,size_t>& current, const std::map<size_t,size_t>& required, size_t num_indicators) {
    int score = 0;
    for (int i = 0; i < num_indicators; i++) {
        int d = required.at(i) - current.at(i);
        if (d < 0) {
            return -1;
        }
        score += d;
    }
    return score;
}

std::map<size_t, size_t> best_buttons2(const std::vector<std::set<size_t>>& buttons, const std::vector<size_t>& joltage_levels) {

    node2 result;

    // Required indicators to be set.
    size_t num_indicators = joltage_levels.size();
    std::map<size_t,size_t> required;
    std::map<size_t,size_t> empty;
    for (int i = 0; i < num_indicators; i++) {
        required[i] = joltage_levels[i];
        empty[i] = 0;
    }
    // Populate first level open list with single button presses.
    std::list<node2> open_list;
    for (size_t bi = 0; bi < buttons.size(); bi++) {
        std::set<size_t> b = buttons[bi];
        auto state1 = apply_button2(b, empty, 1);
        auto score1 = score2(state1, required, num_indicators);
        if (score1 < 0) {
            continue;
        }

        node2 n1;
        n1.actions[bi]++;
        n1.num_actions = 1;
        n1.joltage = state1;
        n1.score = score1;

        bool f_append = true;
        for (auto oi = open_list.begin(); oi != open_list.end(); oi++) {
            if (score1 < oi->score) {
                open_list.emplace(oi, n1);
                f_append = false;
                break;
            }
        }
        if (f_append) {
            open_list.emplace_back(n1);
        }
    }
    // Search till we find perfect match.
    std::set<std::map<size_t,size_t>> closed_list;
    while (!open_list.empty()) {
        auto current = open_list.front();
        open_list.pop_front();
        // Check to see if done.
        if (current.score == 0) {
            result = current;
            break;
        }
        // Make sure this node has not been expanded already.
        if (closed_list.contains(current.actions)) {
            continue;
        }
        closed_list.emplace(current.actions);
        // Expand search from current.
        for (size_t bi = 0; bi < buttons.size(); bi++) {
            auto st2 = apply_button2(buttons[bi], current.joltage, 1);
            auto sc2 = score2(st2, required, num_indicators);
            if (sc2 < 0) {
                continue;
            }

            node2 n2;
            n2.actions = current.actions;
            n2.actions[bi]++;
            n2.num_actions++;
            n2.joltage = st2;
            n2.score = sc2;

            bool f_append = true;
            for (auto oi = open_list.begin(); oi != open_list.end(); oi++) {
                if ((n2.num_actions <= oi->num_actions) && (sc2 < oi->score)) {
                    open_list.emplace(oi, n2);
                    f_append = false;
                    break;
                }
            }
            if (f_append) {
                open_list.emplace_back(n2);
            }
        }
    }
    if (result.num_actions == 0) {
        std::cerr << "Unable to find buttons for given joltage." << std::endl;
    }
    return result.actions;
}

int process_line(const std::string& line, bool f_part1) {
    auto term_strings = split_delim(trim(line), ' ');
    size_t n = term_strings[0].size();

    std::string state = term_strings[0].substr(1, n - 2);
    std::string joltage = term_strings[term_strings.size() - 1];
    std::vector<std::set<size_t>> buttons;
    for (size_t si = 1; si != term_strings.size() - 1; si++) {
        std::set<size_t> indicators;
        for (auto s : split_delim(term_strings[si].substr(1, term_strings[si].size() - 2), ',')) {
            indicators.insert(atoi(s.c_str()));
        }
        buttons.push_back(indicators);
    }
    std::vector<size_t> joltage_levels;
    for (auto s : split_delim(joltage.substr(1, joltage.size() - 2), ',')) {
        joltage_levels.push_back(atoi(s.c_str()));
    }

    if (f_part1) {
        // Determine fewest button presses for given state.
        auto actions = best_buttons1(state, buttons);
        return actions.size();
    }
    else {
        // Determine the fewest button presses to give the prescribed joltage levels.
        auto actions = best_buttons2(buttons, joltage_levels);
        size_t count = 0L;
        for (auto ap : actions) {
            count += ap.second;
        }
        return count;
    }
}

number part1(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    int count = 0;
    int line_num = 1;
    std::cout << "Processing line number: ";
    for (auto line : lines) {
        std::cout << line_num++ << " ";
        std::cout.flush();
        count += process_line(line, true);
    }
    std::cout << std::endl;
    return count;
}

number part2(const char* file_name) {
    std::vector<std::string> lines = read_input_file(file_name);
    int count = 0;
    int line_num = 1;
    std::cout << "Processing line number: ";
    for (auto line : lines) {
        std::cout << line_num++ << " ";
        std::cout.flush();
        count += process_line(line, false);
    }
    std::cout << std::endl;
    return count;
}

int main()
{
    number count = 0;
    std::cout << "Advent of Code 2025\nDay 10 - Factory\n";

    std::cout << "Part 1." << std::endl;
    count = part1("test.txt");
    std::cout << "Minimum number of button presses in test.txt = " << count << std::endl;

    count = part1("input.txt");
    std::cout << "Minimum number of button presses in input.txt = " << count << std::endl;

    std::cout << "Part 2." << std::endl;
    count = part2("test.txt");
    std::cout << "Minimum number of button presses in test.txt = " << count << std::endl;

    count = part2("input.txt");
    std::cout << "Minimum number of button presses in input.txt = " << count << std::endl;

    return 0;
}
