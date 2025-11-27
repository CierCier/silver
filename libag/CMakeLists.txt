cmake_minimum_required(VERSION 3.10)

set(LIBAG_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/libag.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/std/io.cpp
)

option(BUILD_AG_SHARED "Build shared/dynamic libag" ON)
option(BUILD_AG_STATIC "Build static libag" ON)

if(BUILD_AG_SHARED)
    add_library(ag_shared SHARED ${LIBAG_SOURCES})
    target_include_directories(ag_shared PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
    target_compile_definitions(ag_shared
        PRIVATE AG_BUILD
        PUBLIC  AG_SHARED
    )
    set_target_properties(ag_shared PROPERTIES
        OUTPUT_NAME ag
        CXX_VISIBILITY_PRESET hidden
        VISIBILITY_INLINES_HIDDEN YES
    )
endif()

if(BUILD_AG_STATIC)
    add_library(ag_static STATIC ${LIBAG_SOURCES})
    target_include_directories(ag_static PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
    set_target_properties(ag_static PROPERTIES
        OUTPUT_NAME ag_static
    )
endif()

