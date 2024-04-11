# Copyright 2024 Kioshi Morosin <knm@hex.lc>
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/.

cmake_minimum_required(VERSION 3.25)
project(la.h)

option(LA_BUILD_TESTS "Build la.h tests" OFF)

add_library(la.h INTERFACE include/la.h)

target_include_directories(la.h INTERFACE "${PROJECT_SOURCE_DIR}/include")
target_compile_features(la.h INTERFACE cxx_std_20)

if (LA_BUILD_TESTS)
    set(CMAKE_CTEST_ARGUMENTS "--verbose")
    enable_testing()
    add_executable(la_tests test/la_tests.cpp)
    target_link_libraries(la_tests PRIVATE la.h)
    add_test(NAME LATests COMMAND la_tests)
endif()
