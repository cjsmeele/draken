/* Copyright (c) 2018, Chris Smeele
 *
 * This file is part of Draken.
 *
 * Draken is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Draken is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Draken.  If not, see <https://www.gnu.org/licenses/>.
 */
#pragma once

#include "draken.hh"

template<typename A, typename B>
struct pair {
    A a;
    B b;
};

// XXX: This stuff is all WIP.

namespace tuple_impl {

    using namespace tt;

    template<typename T>
    struct tuple_elem;

    template<typename I, typename T>
    struct tuple_elem<list<I,T>> {
        using type = T;
        T elem;
    };

    template<typename I, typename T> const T &get_elem(const tuple_elem<list<I,T>> &te)  { return te.elem; }
    template<typename I, typename T> T &get_elem(tuple_elem<list<I,T>> &te)  { return te.elem; }
    template<typename I, typename T> T &get_elem(tuple_elem<list<I,T>> &&te) { return te.elem; }

    template<typename L, typename... Ts>
    struct tuple_base;
    template<typename... Ls, typename... Ts>
    struct tuple_base<list<Ls...>, Ts...> : public tuple_elem<Ts>... {
        template<ull I> auto get() {
            return get_elem(static_cast<tuple_elem<run<nth<uint<I>>, Ts...>>>(*this));
        }
    };

    template<typename... Ts>
    using base_type = run<enumerate<prepend<list<Ts...>,
                                            lift_rigid<tuple_base>>>,
                          Ts...>;
}

template<typename... Ts>
struct tuple : public tuple_impl::base_type<Ts...> { };

namespace std {
    template<typename... Ts>
    struct tuple_size;
    template<typename... Ts>
    struct tuple_size<tuple<Ts...>> { static constexpr tt::ull value = sizeof...(Ts); };

    template<tt::ull N, typename... Ts>
    struct tuple_element;
    template<tt::ull N, typename... Ts>
    struct tuple_element<N, tuple<Ts...>> {
        using type = tt::run<tt::nth<tt::uint<N>>, Ts...>;
    };
}

// using aoeu = tuple<int,char,int>;
// inline tt::uint<12> ___ = tt::uint<sizeof(aoeu)> {} ;
