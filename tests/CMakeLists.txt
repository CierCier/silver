# Silver Standard Library Tests
# This directory contains integration tests for the Silver standard library

include(SilverUtils)

# Find all .ag test files
file(GLOB STDLIB_TESTS *.ag)

# Create a meta-target to build all stdlib tests
add_custom_target(stdlib_tests)

foreach(TEST_FILE ${STDLIB_TESTS})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    
    # Create the executable target with "stdlib_" prefix to avoid conflicts
    add_silver_executable(stdlib_${TEST_NAME} ${TEST_FILE})
    
    # Make the 'stdlib_tests' target depend on this test
    add_dependencies(stdlib_tests stdlib_${TEST_NAME})
    
    # Add as a CTest test
    add_test(
        NAME stdlib_${TEST_NAME}
        COMMAND $<TARGET_FILE:stdlib_${TEST_NAME}>
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
endforeach()

# Create a convenience target to run all stdlib tests
add_custom_target(run_stdlib_tests
    COMMAND ${CMAKE_CTEST_COMMAND} -R "^stdlib_" --output-on-failure
    DEPENDS stdlib_tests
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Running standard library tests"
)
