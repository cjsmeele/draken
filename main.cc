#include "draken.hh"
#include "tuple.hh"
#include <iostream>

int main() {

    tuple<int,char,int> a { 1, 'a', 2 };

    auto [x, y, z] = a;

    std::cout << x;
    std::cout << y;
    std::cout << z;
    std::cout << '\n';

    using namespace tt;
    [[maybe_unused]]
    list<char, int, long long, short[10]> o = run<sort_on<sizeof_<>>,
                                                  long long, char, short[10], int> {};
    [[maybe_unused]]
    list<char, short[10], int, long long> p = run<sort_on<alignof_<>>,
                                                  long long, short[10], int, char> {};

    return 0;
}
