#pragma once
#include "common.h"
#include <sstream>

class Coord
{
public:
    Coord() : x(0), y(0), z(0) {}
    Coord(number argx, number argy, number argz) : x(argx), y(argy), z(argz) {}
    virtual ~Coord() {}

    // Simple Manhattan distance between 2 objects.
    number distance(const Coord& other) const {
        number dx = x - other.x;
        number dy = y - other.y;
        number dz = z - other.z;
        return (dx * dx + dy * dy + dz * dz);
    }

    std::string display() {
        std::stringstream ss(std::ios_base::openmode::_S_out);
        ss << "(" << x << "," << y << "," << z << ")";
        return ss.str();
    }

    number x, y, z;
};
