CXX = g++
CPPFLAGS = -c -g -std=c++23
LDFLAGS = 
LDLIBS = 
HEADERS =
CSRC = main.cpp common.cpp
OBJECTS = main.o common.o

all: Makefile mk.deps main.exe

mk.deps: 
	$(CXX) -MM *.cpp > mk.deps

main.exe: Makefile mk.deps $(OBJECTS)
	$(CXX) -o main.exe $(OBJECTS)

clean:
	rm -fr *.o a.out main.exe mk.deps
