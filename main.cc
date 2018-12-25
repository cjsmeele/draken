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

    return 0;
}
