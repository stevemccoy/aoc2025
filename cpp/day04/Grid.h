#pragma once
#include "Position.h"
#include <map>
#include <vector>

class Grid
{
public:
	Grid() {}
	virtual ~Grid() { clear(); }

	void setup(const std::vector<std::string>& lines);

	void set(size_t col, size_t row, char value) {
		grid[offset(col, row)] = value;
	}

	char get(size_t col, size_t row) const {
		return grid[offset(col, row)];
	}

	std::vector<Position> neighbours(const Position& p) const { return queen_neighbours(p); }

	void display();

	int count_neighbours_with(const Position& p, char ch);

	char* grid = nullptr;
	size_t num_cols = 0, num_rows = 0;

private:
	void clear();

	size_t offset(size_t col, size_t row) const {
		return row * num_cols + col;
	}

	bool valid(int col, int row) const {
		return ((col >= 0) && (col < num_cols) && (row >= 0) && (row < num_rows));
	}

	bool can_go(const Position& from, int dc, int dr) const;

	std::string row_chars(size_t r) const {
		std::string s;
		s.assign(grid + r * num_cols, num_cols);
		return s;
	}

	std::vector<Position> rook_neighbours(const Position& p) const;

	std::vector<Position> queen_neighbours(const Position& p) const;

};
