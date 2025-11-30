include(SilverUtils)

# Find all .ag files in the examples directory
file(GLOB EXAMPLES *.ag)

# Create a meta-target to build all examples
add_custom_target(examples)

foreach(EXAMPLE ${EXAMPLES})
    get_filename_component(EXAMPLE_NAME ${EXAMPLE} NAME_WE)
    
    # Skip examples that are expected to fail or currently crashing
    if(EXAMPLE_NAME MATCHES "syntax_error|sema_errors")
        message(STATUS "Skipping example ${EXAMPLE_NAME}")
        continue()
    endif()

    # Create the executable target
    add_silver_executable(${EXAMPLE_NAME} ${EXAMPLE})
    
    # Make the 'examples' target depend on this example
    add_dependencies(examples ${EXAMPLE_NAME})
endforeach()
