cmake_minimum_required(VERSION 3.14)

find_package(GTest REQUIRED)
find_package(Threads REQUIRED)

add_executable(agc_tests
  parser_tests.cpp
  codegen_tests.cpp
  comptime_tests.cpp
)

target_link_libraries(agc_tests PRIVATE
  agc_lib
  GTest::gtest_main
  Threads::Threads
)

include(GoogleTest)
gtest_discover_tests(agc_tests)
