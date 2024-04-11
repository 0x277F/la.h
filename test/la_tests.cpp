/*
 * Copyright 2024 Kioshi Morosin <knm@hex.lc>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, you can obtain one at https://mozilla.org/MPL/2.0/.
 */

#include <la.h>
#include "la_tests.h"

using namespace la;

TEST(constructors) {
    constexpr auto x = matrix{ 1 };
    EXPECT(x.cols, std::equal_to, 1);
    EXPECT(x.rows, std::equal_to, 1);
    static_assert(x.cols == x.rows == 1);
    static_assert(x.at<0, 0>() == 1);
    EXPECT((x.at<0, 0>()), std::equal_to, 1);

    constexpr auto y = matrix{ { 1, 2, 3 }, {4, 5, 6} };
    EXPECT(y.cols, std::equal_to, 3);
    EXPECT(y.rows, std::equal_to, 2);
    static_assert(y.cols == 3);
    static_assert(y.rows == 2);

    static_assert(y.at<0, 0>() == 1);
    static_assert(y.at<0, 1>() == 2);
    static_assert(y.at<0, 2>() == 3);
    static_assert(y.at<1, 0>() == 4);
    static_assert(y.at<1, 1>() == 5);
    static_assert(y.at<1, 2>() == 6);
};

TEST(comparison) {
    auto y = matrix{ { 1, 2, 3 }, {4, 5, 6} };
    auto z = matrix{ { 1, 2, 0 }, {4, 5, 6} };
    EXPECT_TRUE(y != z);
    z.at<0, 2>() = 3;
    EXPECT(y, std::equal_to, z);
};

TEST(unary_minus) {
    constexpr auto x = matrix{ { 2, 3 },
                              { -4, 5 } };
    constexpr auto negative_x = -x;
    static_assert(negative_x.at<0, 0>() == -2);
    static_assert(negative_x.at<0, 1>() == -3);
    static_assert(negative_x.at<1, 0>() == 4);
    static_assert(negative_x.at<1, 1>() == -5);

    EXPECT(negative_x, std::equal_to, (matrix{ { -2, -3 }, { 4, -5 } }));
};

TEST(addition) {
    constexpr auto x = matrix{ { 2, 3 },
                               { -4, 5 } };

    constexpr auto y = matrix{ { 9, 0 },
                               { 1, 1 } };
    constexpr auto x_plus_y = x + y;
    static_assert(x_plus_y.at<0, 0>() == 11);
    static_assert(x_plus_y.at<0, 1>() == 3);
    static_assert(x_plus_y.at<1, 0>() == -3);
    static_assert(x_plus_y.at<1, 1>() == 6);

    EXPECT(x_plus_y, std::equal_to, (matrix { { 11, 3 }, { -3, 6 } }));
};

TEST(multiplication) {
    constexpr auto x = matrix{ { 2, 3 },
                               { -4, 5 } };

    constexpr auto y = matrix{ { 9, 0 },
                               { 1, 1 } };
    constexpr auto x_times_y = x * y;
    static_assert(x_times_y.at<0, 0>() == 21);
    static_assert(x_times_y.at<0, 1>() == 3);
    static_assert(x_times_y.at<1, 0>() == -31);
    static_assert(x_times_y.at<1, 1>() == 5);

    EXPECT(x_times_y, std::equal_to, (matrix { { 21, 3 }, { -31, 5 } }));
};

TEST(transpose) {
    constexpr auto x = matrix{ { 2, 0, 9 },
                               { 4, 3, 1 },
                               { 8, 6, 5 }};
    EXPECT(x.T(), std::equal_to, (matrix { { 2, 4, 8 }, { 0, 3, 6 }, { 9, 1, 5 } }));
};

std::vector<testing::controller> testing::all_tests;

int main(int argc, char* argv[]) {
    auto n_tests = testing::all_tests.size();
    std::size_t n_passed = 0;
    std::for_each(testing::all_tests.begin(), testing::all_tests.end(), [&, i = 0](auto& test) mutable {
        std::cout << "[" << ++i << "/" << n_tests <<  "] " << test.name << std::endl;
        std::size_t failed_expects = std::invoke(test);
        if (failed_expects == 0) {
            n_passed++;
            std::cout << "[" << i << "/" << n_tests << "] " << test.name << " passed" << std::endl;
        } else {
            std::cout << "[" << i << "/" << n_tests << "] " << test.name << " failed (" << failed_expects << " items)" << std::endl;
        }
    });
    std::cout << n_passed << "/" << n_tests << " tests passed." << std::endl;
    return static_cast<int>(n_tests - n_passed);
}
