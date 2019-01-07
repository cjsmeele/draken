#CXX      := clang++
CXXFLAGS := -std=c++2a -Wall -Wextra -pedantic -Os -g3 -ftemplate-backtrace-limit=0

main: main.cc draken.hh tuple.hh
	$(CXX) $(CXXFLAGS) -o $@ $<

