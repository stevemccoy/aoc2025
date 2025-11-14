#include "common.h"
#include <iostream>
#include <fstream>

std::vector<std::string> read_input_file(const char* file_name) {
    std::vector<std::string> result;
    std::ifstream infile(file_name, std::ifstream::in);
    std::string line;
    while (std::getline(infile, line, '\n')) {
        result.push_back(line);
    }
    infile.close();
    return result;
}

std::vector<std::string> split_delim(const std::string& line, char delimiter) {
    std::vector<std::string> result;
    size_t startPos = 0, delimPos = line.find_first_of(delimiter);
    while (delimPos != std::string::npos) {
        std::string s1 = line.substr(startPos, delimPos - startPos);
        result.push_back(s1);
        startPos = delimPos + 1;
        delimPos = line.find_first_of(delimiter, startPos);
    }
    result.push_back(line.substr(startPos));
    return result;
}

bool sort_string_by_length(std::string s1, std::string s2) {
    return s1.size() < s2.size();
}

const std::string WHITESPACE = " \n\r\t\f\v";

std::string ltrim(const std::string& s) {
    size_t start = s.find_first_not_of(WHITESPACE);
    return (start == std::string::npos) ? "" : s.substr(start);
}

std::string rtrim(const std::string& s) {
    size_t end = s.find_last_not_of(WHITESPACE);
    return (end == std::string::npos) ? "" : s.substr(0, end + 1);
}

std::string trim(const std::string& s) {
    return rtrim(ltrim(s));
}
