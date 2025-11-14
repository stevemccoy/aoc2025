CXX = g++
CPPFLAGS = -c -g -std=c++23
LDFLAGS = 
LDLIBS = 
HEADERS =
CSRC = main.cpp common.cpp
OBJECTS = main.o common.o

all: Makefile mk.deps main

mk.deps: 
	$(CXX) -MM *.cpp > mk.deps

main: Makefile mk.deps $(OBJECTS)
	g++ -o main $(OBJECTS)

clean:
	rm -fr *.o a.out main mk.deps
