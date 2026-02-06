#pragma once

#include <vector>
#include <string>

// Long number for the counts.
typedef unsigned long long number;

std::vector<std::string> read_input_file(const char* file_name);

std::vector<std::string> split_delim(const std::string& line, char delimiter);

bool sort_string_by_length(std::string s1, std::string s2);

std::string ltrim(const std::string& s);

std::string rtrim(const std::string& s);

std::string trim(const std::string& s);

void trim_remove_empties(std::vector<std::string>& terms);
