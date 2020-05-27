project(DhallCpp LANGUAGES CXX)
cmake_minimum_required(VERSION 3.16)

set(CMAKE_CXX_STANDARD 17)

find_package(pegtl REQUIRED)
add_executable(parser src/main.cpp)
# target_precompile_headers(parser PRIVATE src/grammar.hpp)
target_link_libraries(parser PRIVATE taocpp::pegtl)
