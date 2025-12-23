#pragma once
#include "common.h"
#include <sstream>

class Coord
{
public:
    Coord() : x(0), y(0) {}
    Coord(number argx, number argy) : x(argx), y(argy) {}
    virtual ~Coord() {}

    // Simple Manhattan distance between 2 objects.
    number distance(const Coord& other) const {
        number dx = x - other.x;
        number dy = y - other.y;
        return (dx * dx + dy * dy);
    }

    number rect_area(const Coord& other) const {
        number dx = llabs(x - other.x) + 1;
        number dy = llabs(y - other.y) + 1;
        return (dx * dy);
    }

    std::string display() {
        std::stringstream ss(std::ios_base::openmode::_S_out);
        ss << "(" << x << "," << y << ")";
        return ss.str();
    }

private:
    number x, y;
};
