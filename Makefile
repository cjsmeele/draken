#CXX      := clang++
CXXFLAGS := -std=c++2a -Wall -Wextra -pedantic -Os -g3

main: main.cc draken.hh tuple.hh
	$(CXX) $(CXXFLAGS) -o $@ $<

