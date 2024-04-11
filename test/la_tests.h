/*
 * Copyright 2024 Kioshi Morosin <knm@hex.lc>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, you can obtain one at https://mozilla.org/MPL/2.0/.
 */

#ifndef LA_TESTS_H
#define LA_H

#include <type_traits>
#include <iostream>
#include <functional>

namespace testing {
#pragma clang diagnostic push
#pragma ide diagnostic ignored "google-explicit-constructor"
    template <std::size_t N> struct static_string {
        static constexpr std::size_t _size = N;
        char data[N]{ };

        constexpr explicit(false) static_string(const char (&_data)[N + 1]) {
            std::copy_n(_data, N, data);
        }

        constexpr explicit(false) static_string(std::string_view sv) {
            std::copy_n(sv.cbegin(), N, data);
        }

        constexpr explicit(false) operator std::string_view() const {
            return std::string_view { data, N };
        }

        constexpr bool operator==(std::string_view sv) const {
            return sv == std::string_view { data, N };
        }

        constexpr std::size_t size() const {
            return N;
        }

        template <std::size_t R> constexpr static_string<N + R> operator+(const static_string<R>& rhs) const {
            return static_string<N + R>{ *this, rhs };
        }

        template <std::size_t L, std::size_t R> constexpr static_string<L + R>(const static_string<L>& lhs, const static_string<R>& rhs) {
            auto it = std::copy_n(lhs.data, L, data);
            std::copy_n(rhs.data, R, it);
        }
    };
#pragma clang diagnostic pop

    template <std::size_t N> static_string(const char (&_data)[N]) -> static_string<N-1>;

    struct controller {
        const std::string_view name;
        std::function<void(controller&)> func;
        std::size_t failures = 0;

        template <std::invocable<controller&> Func> controller(const std::string_view _name, Func&& _func): name { _name }, func { std::forward<Func>(_func) } { }

        template <template<class> class Checker = std::equal_to, class L, class R> constexpr bool expect(const L& actual, const R& expected, const std::string&& expr) {
            static_assert(std::is_convertible_v<R, L>);
            using namespace std;
            if (!Checker<L>{ }(actual, expected)) {
                std::cerr << "  failed: " << expr << std::endl << "    expected: " << expected << std::endl << "    actual: " << actual << std::endl;
                failures++;
                return false;
            }
#ifdef LA_VERBOSE_TESTS
            else {
                std::cout << "  passed: " << expr << std::endl;
                if (!std::is_same_v<std::decay_t<L>, bool>) {
                    std::cout << "    value: " << actual << std::endl;
                }
            }
#endif
            return true;
        }

        inline std::size_t operator()() {
            std::invoke(func, *this);
            return failures;
        }
    };
    extern std::vector<controller> all_tests;

    template <static_string Name> struct test_impl {
        static constexpr auto _name = Name;
        template <std::invocable<controller&> Func> explicit(false) inline test_impl(Func&& _func) {
            all_tests.emplace_back(testing::controller{ Name, std::forward<Func>(_func) });
        }
    };
}

template <std::size_t N, std::invocable<testing::controller&> Func> constexpr auto operator|(const testing::static_string<N> after, Func&& _func) {
    (void) after;
    return std::forward<Func>(_func);
}

#define TEST(name, ...) extern const testing::test_impl<#name> test_ ## name = __VA_OPT__((test_ ## __VA_ARGS__)._name |) [](testing::controller& _controller) -> void
#define EXPECT_TRUE(cond) _controller.expect(static_cast<bool>(cond), true, #cond)
#define EXPECT(actual, comparison, expected) _controller.expect<comparison>(actual, expected, #actual " " #comparison " " #expected)
#define REQUIRE_TRUE(cond) do { if (!EXPECT_TRUE(cond)) return; } while(0)
#define REQUIRE(actual, comparison, expected) do { if (!EXPECT(actual, comparison, expected)){ return; } } while(0)

#endif
