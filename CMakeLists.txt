cmake_minimum_required(VERSION 3.10)

project(Silver)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)




# the base silver library (Contains core data structures and algorithms for silver and the silver runtime)
add_subdirectory(libag)

# the silver compiler (agc)
add_subdirectory(agc)