#include "Grid.h"
#include <cstring>
#include <iostream>

void Grid::clear()
{
	if (grid != nullptr) {
		delete[] grid;
		grid = nullptr;
	}
	num_cols = num_rows = 0;
}

void Grid::setup(const std::vector<std::string> &lines)
{
	clear();
    num_rows = lines.size();
    num_cols = lines[0].size();
    size_t num_chars = num_cols * num_rows * sizeof(char);
    grid = new char[num_chars];
    memset(grid, 0, num_chars);
    int r = 0, c = 0;
    for (auto& line : lines) {
        c = 0;
        for (auto ch : line) {
            set(c++, r, ch);
        }
        r++;
    }
}

std::vector<Position> Grid::rook_neighbours(const Position &p) const
{
    std::vector<Position> result;
    if (can_go(p, 0, -1)) {
        result.emplace_back(p.col, p.row - 1);
    }
    if (can_go(p, 0, 1)) {
        result.emplace_back(p.col, p.row + 1);
    }
    if (can_go(p, -1, 0)) {
        result.emplace_back(p.col - 1, p.row);
    }
    if (can_go(p, 1, 0)) {
        result.emplace_back(p.col + 1, p.row);
    }
    return result;
}

std::vector<Position> Grid::queen_neighbours(const Position &p) const
{
    std::vector<Position> result;
	for (int dr = -1; dr <= 1; dr++) {
		for (int dc = -1; dc <= 1; dc++) {
			if ((dc == 0)&&(dr == 0)) {
				continue;
			}
			if (can_go(p, dc, dr)) {
				result.emplace_back(p.col + dc, p.row + dr);
			}
		}
	}
    return result;
}

void Grid::display()
{
    std::cout << std::endl;
    Position p(0, 0);
    for (size_t r = 0; r < num_rows; r++) {
        std::cout << row_chars(r) << std::endl;
    }
    std::cout << std::endl;
}

int Grid::count_neighbours_with(const Position &p, char ch)
{
	int count = 0;
	auto nv = neighbours(p);
	for (Position& q : nv) {
		if (get(q.col,q.row) == ch) {
			count++;
		}
	}
    return count;
}

bool Grid::can_go(const Position &from, int dc, int dr) const
{
    if ((dc < -1) || (dc > 1) || (dr < -1) || (dr > 1)) {
        return false;
    }
	return valid(from.col + dc, from.row + dr);
}

