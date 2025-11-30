cmake_minimum_required(VERSION 3.10)

project(Silver VERSION 0.1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Version configuration
set(SILVER_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(SILVER_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(SILVER_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(SILVER_VERSION "${PROJECT_VERSION}")

# Get git commit hash for dev/nightly builds
find_package(Git QUIET)
if(GIT_FOUND)
    execute_process(
        COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE SILVER_GIT_HASH
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    execute_process(
        COMMAND ${GIT_EXECUTABLE} describe --exact-match --tags HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE SILVER_GIT_TAG
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE GIT_TAG_RESULT
    )
    # If we're on a tagged commit, use clean version; otherwise append commit hash
    if(GIT_TAG_RESULT EQUAL 0)
        set(SILVER_VERSION_FULL "${SILVER_VERSION}")
    else()
        set(SILVER_VERSION_FULL "${SILVER_VERSION}-${SILVER_GIT_HASH}")
    endif()
else()
    set(SILVER_GIT_HASH "unknown")
    set(SILVER_VERSION_FULL "${SILVER_VERSION}")
endif()

message(STATUS "Silver version: ${SILVER_VERSION_FULL}")

# the base silver library (Contains core data structures and algorithms for silver and the silver runtime)
add_subdirectory(libag)

# the silver compiler (agc)
add_subdirectory(agc)

# Add cmake directory to module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Build examples
add_subdirectory(examples)