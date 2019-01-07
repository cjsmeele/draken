/* Copyright (c) 2018, 2019, Chris Smeele
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

// Draken is a continuation-passing-style hobby template metaprogramming library,
// inspired by Kvasir::mpl ( https://github.com/kvasir-io/mpl ),
// but built from the ground up.
//
// There is no cross-over into runtime - all algorithms are to be run at
// compile-time, resulting in types that may be instantiatable at runtime.
//
// This is the project goal:
//
//   Re-inventing the wheel in order to understand what makes bicycles roll.
//
// Some information on design, style and documentation:
//
// 1. I made this library mostly for my hobby and to learn more about template
//    metaprogramming. This being a personal learning project, I do not
//    particularly care about production-level usability.
//    There is currently no external documentation, no extensive tests,
//    and no compatibility guarantees between library revisions.
//    The library is only tested against recent clang and gcc.
//
// 2. We completely ignore the possible existence of a C++ standard library.
//    This means that we do not re-use any types or functions from the standard
//    library, and we additionally will NOT strive to use C++ standard library
//    terminology for our functions and types (e.g. for sequence operations).
//
// 3. Compile-time performance is nice, but as long as I do not run into any
//    performance issues while using this library for my own projects, I will
//    not add optimizations that reduce legibility of the library source code.
//    I like algo implementations to be straight-forward and easy to read
//    (insofar template code can be considered readable, of course ;-).
//
// 4. Features are added on an as-needed/when-needed basis. This library is
//    always "complete" in the sense that it's never missing a feature that I
//    personally need to use at a given point in time.

// Let's call our namespace tt ("template toolkit") for now.

namespace tt {

// Compile-time values {{{

using ull = unsigned long long;
using sll =   signed long long;

template<ull I> struct uint { static constexpr ull value = I; };
template<sll I> struct sint { static constexpr sll value = I; };

template<typename... Ts>
struct list { };

template<bool B> struct bool_ { static constexpr bool value = B; };
using true_  = bool_<true>;
using false_ = bool_<false>;

// }}}
// Fundamentals {{{

// Metafunction terminators.
struct return_one  { template<typename T>     using type = T; };
struct return_list { template<typename... Ts> using type = list<Ts...>; };

// Take a list and turn it into a parameter pack.
// (does not touch any other item in the input pack)
template<typename C = return_list>
struct unlist {
    // If it's not a list, don't do anything.
    template<typename T, typename... Ys> struct impl {
        using type = typename C::template type<T, Ys...>;
    };
    template<typename... Ts, typename... Ys> struct impl<list<Ts...>, Ys...> {
        using type = typename C::template type<Ts..., Ys...>;
    };

    template<typename... Ts> using type = typename impl<Ts...>::type;
};

// Pass on the input unmodified.
template<typename C = return_one>
struct identity { template<typename... Ts> using type = typename C::template type<Ts...>; };

// Ignores the input and passes on T instead.
template<typename T, typename C = return_one>
struct const_ { template<typename... Ts> using type = typename C::template type<T>; };

// }}}
// Function tooling {{{

// Run a metafunction.
template<typename F, typename... Ts>
using run = typename F::template type<Ts...>;

// Lifts a template with non-pack arguments into a metafunction that is run<>-able.
// (needed because a pack cannot be expanded into non-pack parameters)
template<template<typename...> typename E,
         typename C = return_one>
struct lift_rigid {
    // Specializations can be added as needed, when templates with more fixed
    // arguments need to be called.
    template<typename... Ts>
    struct impl;
    template<typename T1, typename... Ts>
    struct impl<T1,Ts...> { using type = E<T1,Ts...>; };
    template<typename T1, typename T2, typename... Ts>
    struct impl<T1,T2,Ts...> { using type = E<T1,T2,Ts...>; };
    template<typename T1, typename T2, typename T3, typename... Ts>
    struct impl<T1,T2,T3,Ts...> { using type = E<T1,T2,T3,Ts...>; };
    template<typename T1, typename T2, typename T3, typename T4, typename... Ts>
    struct impl<T1,T2,T3,T4,Ts...> { using type = E<T1,T2,T3,T4,Ts...>; };
    template<typename T1, typename T2, typename T3, typename T4, typename T5, typename... Ts>
    struct impl<T1,T2,T3,T4,T5,Ts...> { using type = E<T1,T2,T3,T4,T5,Ts...>; };
    template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename... Ts>
    struct impl<T1,T2,T3,T4,T5,T6,Ts...> { using type = E<T1,T2,T3,T4,T5,T6,Ts...>; };
    template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts>
    struct impl<T1,T2,T3,T4,T5,T6,T7,Ts...> { using type = E<T1,T2,T3,T4,T5,T6,T7,Ts...>; };
    template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename... Ts>
    struct impl<T1,T2,T3,T4,T5,T6,T7,T8,Ts...> { using type = E<T1,T2,T3,T4,T5,T6,T7,T8,Ts...>; };

    // Run this Vim macro on the last "template<..." line above to add a new specialization.
    // 2yyjp^f>F,v2bhyf,pbj^f>F,byf,f,pb$F>F,byf,f,pbk^

    template<typename... Ts> using type = typename C::template type<typename impl<Ts...>::type>;
};

// Add a continuation to a metafunction.
// (not sure about the terminology here)
template<typename F,
         typename C>
struct after {
    template<typename... Ts>
    using type = typename C::template type<run<F, Ts...>>;
};

// }}}
// Conditionals {{{

template<typename B> struct if_impl;
template<> struct if_impl<true_> {
    template<typename F1,
             typename F2,
             typename... Ts>
    using type = run<F1,Ts...>;
};
template<> struct if_impl<false_> {
    template<typename F1,
             typename F2,
             typename... Ts>
    using type = run<F2,Ts...>;
};

// Conditionally continues in a then or else branch.
template<typename FP,
         typename Then,
         typename Else = return_one>
struct if_ {
    template<typename... Ts>
    using type = typename if_impl<run<FP,Ts...>>::template type<Then, Else, Ts...>;
};

// }}}
// Functional {{{

// Maps the given metafunction over its input.
template<typename F, typename C = return_list>
struct map {
    // Simply run the given function on each T.
    template<typename... Ts>
    using type = typename C::template type<run<F,Ts>...>;
};

// Folds the given metaoperator over its input.
// A0 is the initial accumulator type.
template<typename F,
         typename A0,
         typename C = return_one>
struct foldl {
    template<typename A, typename... Ts> struct impl;
    template<typename A>
    struct impl<A>         { using type = typename C::template type<A>; };
    template<typename A, typename T, typename... Ts>
    struct impl<A,T,Ts...> { using type = typename impl<run<F,A,T>,Ts...>::type; };

    template<typename... Ts>
    using type = typename impl<A0,Ts...>::type;
};

// Filters its input based on a metapredicate.
template<typename FP, typename C = return_list>
struct filter {
    template<typename A, typename... Ts>
    struct impl;
    template<typename... A>
    struct impl<list<A...>> { using type = typename C::template type<A...>; };
    template<typename... A, typename T, typename... Ts>
    struct impl<list<A...>, T, Ts...> {
        using type = run<if_<FP,
                             const_<typename impl<list<A..., T>, Ts...>::type>,
                             const_<typename impl<list<A...>,    Ts...>::type>>,
                         T>;
    };

    template<typename... Ts>
    using type = typename impl<list<>, Ts...>::type;
};

// template<typename C = return_list>
// struct zip {
// };

template<typename C = return_list>
struct enumerate {
    template<ull I, typename A, typename... Ts>
    struct impl;
    template<ull I, typename... As>
    struct impl<I, list<As...>> {
        using type = typename C::template type<As...>;
    };
    template<ull I, typename... As, typename T, typename... Ts>
    struct impl<I, list<As...>, T, Ts...> {
        using type = typename impl<(I+1), list<As..., list<uint<I>,T>>, Ts...>::type;
    };

    template<typename... Ts>
    using type = typename impl<0, list<>, Ts...>::type;
};

// }}}
// Predicates {{{

// Type equality.
template<typename T1, typename T2> struct equal__         { static constexpr bool value = false; };
template<typename T1>              struct equal__<T1, T1> { static constexpr bool value = true;  };
template<typename T1, typename T2> using  equal_ = bool_<equal__<T1,T2>::value>;
template<typename C = return_one>  using  equal  = lift_rigid<equal_, C>;

template<typename T>              struct is_list__              { static constexpr bool value = false; };
template<typename... Ts>          struct is_list__<list<Ts...>> { static constexpr bool value = true;  };
template<typename T>              using  is_list_ = bool_<is_list__<T>::value>;
template<typename C = return_one> using  is_list  = lift_rigid<is_list_, C>;

// Integer predicates {{{

#define DEF_BINARY_PRED(name, op) \
    template<typename T1, typename T2> using name##_ = bool_<((T1::value) op (T2::value))>; \
    template<typename C = return_one>  using name = lift_rigid<name##_, C>;

#define DEF_UNARY_PRED(name, op) \
    template<typename T1> using name##_ = bool_<(op (T1::value))>; \
    template<typename C = return_one>  using name = lift_rigid<name##_, C>;

DEF_BINARY_PRED(le, <=)
DEF_BINARY_PRED(lt, < )
DEF_BINARY_PRED(ge, >=)
DEF_BINARY_PRED(gt, > )
DEF_BINARY_PRED(eq, ==)
DEF_BINARY_PRED(ne, !=)

template<typename N> constexpr bool fneven_(N x) { return (x & 1) == 0; }
template<typename N> constexpr bool fnodd_ (N x) { return (x & 1) == 1; }
template<typename N> constexpr bool fnnot_ (N x) { return !x; }

DEF_UNARY_PRED(even, fneven_)
DEF_UNARY_PRED(odd,  fnodd_)
DEF_UNARY_PRED(not_, fnnot_)

#undef DEF_UNARY_PRED
#undef DEF_BINARY_PRED

// }}}
// }}}
// Basic pack operations {{{

template<typename T, typename C = return_list>
struct prepend { template<typename... Ts> using type = typename C::template type<T, Ts...>; };

template<typename T, typename C = return_list>
struct append  { template<typename... Ts> using type = typename C::template type<Ts..., T>; };

template<typename FP, typename F, typename C = return_list>
using replace = map<if_<FP, F>, C>;

template<typename T1, typename T2>
using join2_ = run<unlist<prepend<T1, unlist<>>>, T2>;

template<typename C = return_list>
using join2  = lift_rigid<join2_, C>;

// Shallow unlist of all lists in the input pack.
template<typename C = return_list>
using join = foldl<join2<unlist<>>, list<>, unlist<C>>;

// }}}
// Integer operations {{{

// Definitions of binary operators that work on unsigned values by default,
// but result in signed values when either of the operands is signed.

#define DEF_BINOP(name, op) \
    template<typename T1, typename T2> struct name##__; \
    template<ull V1, ull V2> struct name##__<uint<V1>, uint<V2>> { using type = uint<(                (V1) op                 (V2))>; }; \
    template<ull V1, sll V2> struct name##__<uint<V1>, sint<V2>> { using type = sint<(static_cast<sll>(V1) op                 (V2))>; }; \
    template<sll V1, ull V2> struct name##__<sint<V1>, uint<V2>> { using type = sint<(                (V1) op static_cast<sll>(V2))>; }; \
    template<sll V1, sll V2> struct name##__<sint<V1>, sint<V2>> { using type = sint<(                (V1) op                 (V2))>; }; \
    template<typename T1, typename T2> using name##_ = typename name##__<T1, T2>::type; \
    template<typename C = return_one>  using name    = lift_rigid<name##_, C>;

// Definitions of unary operators.
#define DEF_UNOP(name, op) \
    template<typename T1> struct name##__; \
    template<ull V1> struct name##__<uint<V1>> { using type = uint<(op (V1))>; }; \
    template<sll V1> struct name##__<sint<V1>> { using type = sint<(op (V1))>; }; \
    template<typename T1>             using name##_ = typename name##__<T1>::type; \
    template<typename C = return_one> using name    = lift_rigid<name##_, C>;

DEF_BINOP(    add, +)
DEF_BINOP(    sub, -)
DEF_BINOP(    mul, *)
DEF_BINOP(    div, /)
DEF_BINOP(    mod, %)
DEF_BINOP(bin_and, &)
DEF_BINOP(bin_or,  |)
DEF_BINOP(bin_xor, ^)
DEF_BINOP(bin_shr, >>)
DEF_BINOP(bin_shl, <<)

DEF_UNOP(bin_negate, ~)
DEF_UNOP(logical_negate, !)

template<typename N> constexpr ull fnabs_ (N x) { return x < 0 ? -x : x; }
template<typename N> constexpr sll fnsgn_ (N x) { return x < 0 ? -1 : 1; }
template<typename N> constexpr sll fnsucc_(N x) { return x + 1; }

DEF_UNOP(sgn,  fnsgn_)
DEF_UNOP(abs,  fnabs_)
DEF_UNOP(succ, fnsucc_)

template<typename C = return_one>
using negate = prepend<sint<(-1)>, mul<C>>;

#undef DEF_UNOP
#undef DEF_BINOP

template<typename C = return_one>
using increment = prepend<uint<1>, add<C>>;

template<typename C = return_one>
using decrement = append<uint<1>, sub<C>>;

// }}}
// Other pack operations {{{

template<typename N, typename C = return_list>
struct take {
    template<ull I, typename A, typename... Ts> struct impl;

    template<ull I, typename... As>
    struct impl<I, list<As...>> { using type = typename C::template type<As...>; };

    template<typename... As, typename T, typename... Ts>
    struct impl<0, list<As...>, T, Ts...> { using type = typename C::template type<As...>; };

    template<ull I, typename... As, typename T, typename... Ts>
    struct impl<I, list<As...>, T, Ts...> {
        using type = typename impl<(I-1), list<As..., T>, Ts...>::type;
    };

    template<typename... Ts>
    using type = typename impl<N::value, list<>, Ts...>::type;
};

template<typename N, typename C = return_list>
struct drop {
    template<ull I, typename... Ts> struct impl;

    template<ull I>
    struct impl<I> { using type = typename C::template type<>; };

    template<typename T, typename... Ts>
    struct impl<0, T, Ts...> { using type = typename C::template type<T, Ts...>; };

    template<ull I, typename T, typename... Ts>
    struct impl<I, T, Ts...> { using type = typename impl<(I-1), Ts...>::type; };

    template<typename... Ts>
    using type = typename impl<N::value, Ts...>::type;
};

template<typename C = return_one>
struct head {
    template<typename... Ts>             struct impl;
    template<typename T, typename... Ts> struct impl<T,Ts...> { using type = typename C::template type<T>; };
    template<typename... Ts> using type = typename impl<Ts...>::type;
};

template<typename C = return_list>
struct tail {
    template<typename... Ts>             struct impl;
    template<typename T, typename... Ts> struct impl<T,Ts...> { using type = typename C::template type<Ts...>; };
    template<typename... Ts> using type = typename impl<Ts...>::type;
};

template<typename C = return_one> using sum     = foldl<add<>, uint<0>, C>;
template<typename C = return_one> using product = foldl<mul<>, uint<1>, C>;

template<typename C = return_one>
struct size { template<typename... Ts> using type = uint<sizeof...(Ts)>; };

template<typename N, typename C = return_one>
struct nth {
    template<typename... Ts>
    struct impl {
        static_assert(N::value >= 0);
        static_assert(N::value <  sizeof...(Ts));

        using type = run<drop<N, head<C>>, Ts...>;
    };

    template<typename... Ts>
    using type = typename impl<Ts...>::type;
};

// Generate integer sequences.
template<typename N, typename C = return_list>
struct iota0 {
    template<ull I, typename... Ns>
    struct impl           { using type = typename impl<(I-1),uint<(I-1)>,Ns...>::type; };
    template<typename... Ns>
    struct impl<0, Ns...> { using type = typename C::template type<Ns...>; };

    template<typename... Ts>
    using type = typename impl<N::value>::type;
};

// Generate integer sequences starting at 1.
template<typename N, typename C = return_list>
using iota1 = iota0<N,map<increment<>,C>>;

// Insert a type at a given position in a pack.
template<typename I, typename T, typename C = return_list>
struct insert {
    template<typename X, typename Y>
    struct impl;
    template<typename... Xs, typename... Ys>
    struct impl<list<Xs...>, list<Ys...>> {
        using type = typename C::template type<Xs..., T, Ys...>;
    };
    template<typename... Ts>
    using type = typename impl<run<take<I>, Ts...>,
                               run<drop<I>, Ts...>>::type;
};

// }}}
// Advanced operators {{{

template<typename C = return_list,
         typename... Fs>
struct fork {
    template<typename... Ts>
    using type = typename C::template type<run<Fs, Ts...>...>;
};

// }}}
// Algorithms {{{

template<typename T1, typename T2> using max_ = run<if_<ge<>, const_<T1>, const_<T2>>, T1, T2>;
template<typename T1, typename T2> using min_ = run<if_<le<>, const_<T1>, const_<T2>>, T1, T2>;
template<typename C = return_one>  using max  = lift_rigid<max_, C>;
template<typename C = return_one>  using min  = lift_rigid<min_, C>;

template<typename C = return_one> using maximum = foldl<max<>, uint<0>, C>;
template<typename C = return_one> using minimum = foldl<min<>, uint<0>, C>;

template<typename T> using sizeof__  = uint< sizeof(T)>;
template<typename T> using alignof__ = uint<alignof(T)>;
template<typename C = return_one> using sizeof_  = lift_rigid<sizeof__,  C>;
template<typename C = return_one> using alignof_ = lift_rigid<alignof__, C>;

// Partition a pack into two lists, based on a predicate.
template<typename FP, typename C = return_list>
using partition = fork<C,
                       filter<FP>,
                       filter<after<FP, not_<>>>>;

// Insertion sort.
template<typename C = return_list> struct sort {

    template<typename T, typename... Ts>
    using insert = run<partition<append<T, le<>>
                                ,tt::insert<uint<1>, list<T>
                                           ,join<>>>
                      ,Ts...>;

    template<typename A, typename... Ts>
    struct impl;
    template<typename... A>
    struct impl<list<A...>> { using type = typename C::template type<A...>; };
    template<typename... A, typename T, typename... Ts>
    struct impl<list<A...>, T, Ts...> {
        using type = typename impl<insert<T, A...>, Ts...>::type;
    };

    template<typename... Ts>
    using type = typename impl<list<>, Ts...>::type;
};

// }}}
// Ad-hoc tests {{{

#if 1

namespace {
    struct t1 {};
    struct t2 {};
    struct t3 {};
    struct t4 {};

#define PASTE2(x,y) x##y
#define PASTE1(x,y) PASTE2(x,y)
#define TEST_NAME PASTE1(test__, __LINE__)

// va args needed because preproc will not parse ',' within template parameters correctly :-)
// (yes, this is macro abuse, but it's sufficient for now)
#define ASSERT_EQ(type, ...) constexpr type TEST_NAME [[maybe_unused]] = __VA_ARGS__ {};

    // take and drop
    ASSERT_EQ(list<  >, run<take<uint<5>>>)
    ASSERT_EQ(list<  >, run<take<uint<0>>>)
    ASSERT_EQ(list<  >, run<take<uint<0>>, t1>)
    ASSERT_EQ(list<t1>, run<take<uint<1>>, t1>)
    ASSERT_EQ(list<t1>, run<take<uint<1>>, t1,t2>)
    ASSERT_EQ(list<  >, run<drop<uint<5>>>)
    ASSERT_EQ(list<  >, run<drop<uint<0>>>)
    ASSERT_EQ(list<t1>, run<drop<uint<0>>, t1>)
    ASSERT_EQ(list<  >, run<drop<uint<1>>, t1>)
    ASSERT_EQ(list<t2>, run<drop<uint<1>>, t1,t2>)

    ASSERT_EQ(uint<2>, run<drop<uint<1>, take<uint<2>, sum<>>>,
                           uint<999>, uint<1>, uint<1>, uint<999>>)

    // indexing
    ASSERT_EQ(t1, run<nth<uint<0>>, t1,t2,t3>)
    ASSERT_EQ(t2, run<nth<uint<1>>, t1,t2,t3>)
    ASSERT_EQ(t3, run<nth<uint<2>>, t1,t2,t3>)

    // branching
    ASSERT_EQ(uint<0xfeedbeef>,
              run<if_<ge<>,
                      const_<uint<0xdeaddead>>,
                      const_<uint<0xfeedbeef>>>,
                    uint<2>,
                    uint<3>>)

    // integer sequences
    ASSERT_EQ(uint<10>, run<iota1<uint<4>, sum<>>>)
    ASSERT_EQ(uint< 6>, run<iota0<uint<4>, sum<>>>)

    // TODO: Rewrite tests below:

    constexpr list<t2,t3> _3 = run<tail<>, t1,t2,t3> {};
    constexpr t2 _4          = run<tail<head<>>, t1,t2,t3> {};

    constexpr uint<5> _5 = run<maximum<>, uint<2>, uint<5>, uint<4>> {};

    constexpr list<uint<2>, uint<4>> _6 = run<filter<even<>>,
                                              uint<2>, uint<5>, uint<4>> {};
    constexpr list<uint<3>, uint<5>> _7 = run<filter<even<>, map<increment<>>>,
                                              uint<2>, uint<5>, uint<4>> {};

    constexpr uint<10> _8 = run<filter<odd<>,
                                       map<increment<>,
                                           sum<>>>,
                                uint<3>, uint<5>, uint<4>> {};

    constexpr sint<(-10)> _9 = run<unlist<filter<odd<>,
                                                 map<increment<>,
                                                     sum<negate<>>>>>,
                                   list<uint<3>, uint<5>>, uint<4>> {};

    using double_if_even = if_<even<>, prepend<uint<2>, mul<>>>;

    ASSERT_EQ(sint<(- 5)>, run<double_if_even, sint<(- 5)>>)
    ASSERT_EQ(sint<(-20)>, run<double_if_even, sint<(-10)>>)

    using doubled_plus_incremented = fork<add<>, // <- is run on the results of the branches below.
                                          prepend<uint<2>, mul<>>,
                                          increment<>>;

    ASSERT_EQ(sint<(25)>, run<doubled_plus_incremented,
                              sint<(8)>>)

    constexpr list<list<uint<1>, uint<3>>
                  ,list<uint<0>, uint<2>>> _parted
              = run<iota0<uint<4>, partition<odd<>>>> {};

    constexpr list<uint<1>, uint<2>, uint<3>> _10
              = run<insert<uint<0>, uint<1>>, uint<2>, uint<3>> {};
    constexpr list<uint<1>, uint<2>, uint<3>> _11
              = run<insert<uint<1>, uint<2>>, uint<1>, uint<3>> {};
    constexpr list<uint<1>, uint<2>, uint<3>> _12
              = run<insert<uint<2>, uint<3>>, uint<1>, uint<2>> {};

    constexpr list<uint<1>, uint<2>, uint<3>, uint<4>> _joined
              = run<join<>, list<uint<1>>, uint<2>, list<uint<3>, uint<4>>> {};

    constexpr list<uint<1>, uint<2>, uint<3>> _sorted1
              = run<sort<>, uint<1>, uint<2>, uint<3>> {};
    constexpr list<uint<1>, uint<2>, uint<3>> _sorted2
              = run<sort<>, uint<1>, uint<3>, uint<2>> {};
    constexpr list<uint<1>, uint<2>, uint<3>> _sorted3
              = run<sort<>, uint<3>, uint<2>, uint<1>> {};
    constexpr list<uint<1>, uint<2>, uint<2>, uint<3>> _sorted4
              = run<sort<>, uint<3>, uint<2>, uint<1>, uint<2>> {};

#undef TEST_NAME
#undef PASTE1
#undef PASTE2
#undef ASSERT_EQ
}

#endif
// }}}

}
