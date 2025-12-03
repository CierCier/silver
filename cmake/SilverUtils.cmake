# =============================================================================
# Silver CMake Utilities
# =============================================================================
# Provides helper functions for building Silver programs with CMake.
#
# Functions:
#   add_silver_executable       - Build a Silver executable
#   add_silver_library          - Build a Silver library (object files)
#   silver_syntax_check         - Validate Silver source files
#   silver_ast_dump             - Generate AST dump for debugging
#   silver_set_default_options  - Set default compiler options for all targets
#
# Variables (set before including this module to customize):
#   SILVER_COMPILER             - Path to agc compiler (default: build target)
#   SILVER_DEFAULT_INCLUDE_DIRS - Additional include directories
#   SILVER_DEFAULT_LINK_DIRS    - Additional library directories
#   SILVER_DEFAULT_LINK_LIBS    - Libraries to link by default
#   SILVER_DEFAULT_DEFINES      - Preprocessor definitions
#   SILVER_VERBOSE              - Enable verbose compilation output
#   SILVER_STATIC_LINK          - Link statically by default
# =============================================================================

# Default paths if building in-tree
if(NOT DEFINED SILVER_LIBAG_DIR)
    set(SILVER_LIBAG_DIR "${CMAKE_BINARY_DIR}/libag")
endif()

if(NOT DEFINED SILVER_LIBAG_INCLUDE_DIR)
    set(SILVER_LIBAG_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/libag/include")
endif()

# Global defaults (can be overridden per-target)
set(SILVER_DEFAULT_INCLUDE_DIRS "" CACHE STRING "Default include directories for Silver compilation")
set(SILVER_DEFAULT_LINK_DIRS "" CACHE STRING "Default library directories for Silver linking")
set(SILVER_DEFAULT_LINK_LIBS "" CACHE STRING "Default libraries to link with Silver programs")
set(SILVER_DEFAULT_DEFINES "" CACHE STRING "Default preprocessor definitions for Silver")
set(SILVER_VERBOSE OFF CACHE BOOL "Enable verbose Silver compilation")
set(SILVER_STATIC_LINK OFF CACHE BOOL "Link Silver programs statically by default")
set(SILVER_DEFAULT_BACKEND "llvm" CACHE STRING "Default codegen backend (llvm, js, csharp)")

# =============================================================================
# add_silver_executable
# =============================================================================
# Compile a Silver source file into an executable.
#
# Usage:
#   add_silver_executable(<target> <source>
#       [OUTPUT_NAME <name>]
#       [INCLUDE_DIRS <dir>...]
#       [LINK_DIRS <dir>...]
#       [LINK_LIBS <lib>...]
#       [DEFINES <def>...]
#       [BACKEND <backend>]
#       [STATIC]
#       [VERBOSE]
#       [DEPENDS <target>...]
#       [EXTRA_ARGS <arg>...]
#   )
#
# Arguments:
#   target       - Name of the CMake target to create
#   source       - Silver source file (.ag)
#   OUTPUT_NAME  - Override the output executable name
#   INCLUDE_DIRS - Additional include directories (-I)
#   LINK_DIRS    - Additional library search directories (-L)
#   LINK_LIBS    - Additional libraries to link (-l)
#   DEFINES      - Preprocessor definitions (-D)
#   BACKEND      - Code generation backend (llvm, js, csharp)
#   STATIC       - Link statically (--static)
#   VERBOSE      - Show verbose compilation output (-v)
#   DEPENDS      - Additional CMake target dependencies
#   EXTRA_ARGS   - Additional arguments to pass to agc
# =============================================================================
function(add_silver_executable TARGET_NAME SOURCE_FILE)
    cmake_parse_arguments(PARSE_ARGV 2 SILVER
        "STATIC;VERBOSE"
        "OUTPUT_NAME;BACKEND"
        "INCLUDE_DIRS;LINK_DIRS;LINK_LIBS;DEFINES;DEPENDS;EXTRA_ARGS"
    )

    # Determine output file name
    if(SILVER_OUTPUT_NAME)
        set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${SILVER_OUTPUT_NAME}")
    else()
        set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}")
    endif()

    # Compiler target/executable
    if(DEFINED SILVER_COMPILER)
        set(AGC_CMD "${SILVER_COMPILER}")
        set(AGC_DEP "")
    else()
        set(AGC_TARGET agc)
        set(AGC_CMD "$<TARGET_FILE:${AGC_TARGET}>")
        set(AGC_DEP ${AGC_TARGET})
    endif()

    # Build argument list
    set(AGC_ARGS "")

    # Add source file
    get_filename_component(SOURCE_ABS "${SOURCE_FILE}" ABSOLUTE)
    list(APPEND AGC_ARGS "${SOURCE_ABS}")

    # Output file
    list(APPEND AGC_ARGS "-o" "${OUTPUT_FILE}")

    # Include directories (built-in + defaults + per-target)
    list(APPEND AGC_ARGS "-I" "${SILVER_LIBAG_INCLUDE_DIR}")
    foreach(dir ${SILVER_DEFAULT_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()
    foreach(dir ${SILVER_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()

    # Library directories (built-in + defaults + per-target)
    list(APPEND AGC_ARGS "-L" "${SILVER_LIBAG_DIR}")
    foreach(dir ${SILVER_DEFAULT_LINK_DIRS})
        list(APPEND AGC_ARGS "-L" "${dir}")
    endforeach()
    foreach(dir ${SILVER_LINK_DIRS})
        list(APPEND AGC_ARGS "-L" "${dir}")
    endforeach()

    # Link libraries
    foreach(lib ${SILVER_DEFAULT_LINK_LIBS})
        list(APPEND AGC_ARGS "-l" "${lib}")
    endforeach()
    foreach(lib ${SILVER_LINK_LIBS})
        list(APPEND AGC_ARGS "-l" "${lib}")
    endforeach()

    # Preprocessor definitions
    foreach(def ${SILVER_DEFAULT_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()
    foreach(def ${SILVER_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()

    # Backend selection
    if(SILVER_BACKEND)
        list(APPEND AGC_ARGS "--emit=${SILVER_BACKEND}")
    elseif(SILVER_DEFAULT_BACKEND)
        list(APPEND AGC_ARGS "--emit=${SILVER_DEFAULT_BACKEND}")
    endif()

    # Static linking
    if(SILVER_STATIC OR SILVER_STATIC_LINK)
        list(APPEND AGC_ARGS "--static")
    endif()

    # Verbose output
    if(SILVER_VERBOSE OR SILVER_VERBOSE)
        list(APPEND AGC_ARGS "-v")
    endif()

    # Extra arguments (pass-through)
    foreach(arg ${SILVER_EXTRA_ARGS})
        list(APPEND AGC_ARGS "${arg}")
    endforeach()

    # Build dependencies list
    set(ALL_DEPS ${AGC_DEP} ${SOURCE_ABS})
    foreach(dep ${SILVER_DEPENDS})
        list(APPEND ALL_DEPS ${dep})
    endforeach()

    add_custom_command(
        OUTPUT ${OUTPUT_FILE}
        COMMAND ${AGC_CMD} ${AGC_ARGS}
        DEPENDS ${ALL_DEPS}
        COMMENT "Compiling Silver executable: ${TARGET_NAME}"
        VERBATIM
    )

    add_custom_target(${TARGET_NAME} ALL DEPENDS ${OUTPUT_FILE})

    # Store properties for introspection
    set_target_properties(${TARGET_NAME} PROPERTIES
        SILVER_SOURCE_FILE "${SOURCE_ABS}"
        SILVER_OUTPUT_FILE "${OUTPUT_FILE}"
    )
endfunction()

# =============================================================================
# add_silver_library
# =============================================================================
# Compile Silver source files to object files (for linking with C/C++).
#
# Usage:
#   add_silver_library(<target> <source>...
#       [INCLUDE_DIRS <dir>...]
#       [DEFINES <def>...]
#       [BACKEND <backend>]
#       [VERBOSE]
#       [DEPENDS <target>...]
#       [EXTRA_ARGS <arg>...]
#   )
# =============================================================================
function(add_silver_library TARGET_NAME)
    cmake_parse_arguments(PARSE_ARGV 1 SILVER
        "VERBOSE"
        "BACKEND"
        "SOURCES;INCLUDE_DIRS;DEFINES;DEPENDS;EXTRA_ARGS"
    )

    # Remaining unparsed args are source files
    set(SOURCE_FILES ${SILVER_UNPARSED_ARGUMENTS} ${SILVER_SOURCES})

    if(NOT SOURCE_FILES)
        message(FATAL_ERROR "add_silver_library: No source files specified for target ${TARGET_NAME}")
    endif()

    # Compiler target/executable
    if(DEFINED SILVER_COMPILER)
        set(AGC_CMD "${SILVER_COMPILER}")
        set(AGC_DEP "")
    else()
        set(AGC_TARGET agc)
        set(AGC_CMD "$<TARGET_FILE:${AGC_TARGET}>")
        set(AGC_DEP ${AGC_TARGET})
    endif()

    set(OBJECT_FILES "")

    foreach(SOURCE_FILE ${SOURCE_FILES})
        get_filename_component(SOURCE_ABS "${SOURCE_FILE}" ABSOLUTE)
        get_filename_component(SOURCE_NAME "${SOURCE_FILE}" NAME_WE)
        set(OBJECT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}_${SOURCE_NAME}.o")

        # Build argument list
        set(AGC_ARGS "")
        list(APPEND AGC_ARGS "${SOURCE_ABS}")
        list(APPEND AGC_ARGS "-c")
        list(APPEND AGC_ARGS "-o" "${OBJECT_FILE}")

        # Include directories
        list(APPEND AGC_ARGS "-I" "${SILVER_LIBAG_INCLUDE_DIR}")
        foreach(dir ${SILVER_DEFAULT_INCLUDE_DIRS})
            list(APPEND AGC_ARGS "-I" "${dir}")
        endforeach()
        foreach(dir ${SILVER_INCLUDE_DIRS})
            list(APPEND AGC_ARGS "-I" "${dir}")
        endforeach()

        # Preprocessor definitions
        foreach(def ${SILVER_DEFAULT_DEFINES})
            list(APPEND AGC_ARGS "-D" "${def}")
        endforeach()
        foreach(def ${SILVER_DEFINES})
            list(APPEND AGC_ARGS "-D" "${def}")
        endforeach()

        # Backend selection
        if(SILVER_BACKEND)
            list(APPEND AGC_ARGS "--emit=${SILVER_BACKEND}")
        elseif(SILVER_DEFAULT_BACKEND)
            list(APPEND AGC_ARGS "--emit=${SILVER_DEFAULT_BACKEND}")
        endif()

        # Verbose output
        if(SILVER_VERBOSE OR SILVER_VERBOSE)
            list(APPEND AGC_ARGS "-v")
        endif()

        # Extra arguments
        foreach(arg ${SILVER_EXTRA_ARGS})
            list(APPEND AGC_ARGS "${arg}")
        endforeach()

        # Dependencies
        set(ALL_DEPS ${AGC_DEP} ${SOURCE_ABS})
        foreach(dep ${SILVER_DEPENDS})
            list(APPEND ALL_DEPS ${dep})
        endforeach()

        add_custom_command(
            OUTPUT ${OBJECT_FILE}
            COMMAND ${AGC_CMD} ${AGC_ARGS}
            DEPENDS ${ALL_DEPS}
            COMMENT "Compiling Silver object: ${SOURCE_NAME}.o"
            VERBATIM
        )

        list(APPEND OBJECT_FILES ${OBJECT_FILE})
    endforeach()

    add_custom_target(${TARGET_NAME} ALL DEPENDS ${OBJECT_FILES})

    set_target_properties(${TARGET_NAME} PROPERTIES
        SILVER_OBJECT_FILES "${OBJECT_FILES}"
    )
endfunction()

# =============================================================================
# silver_syntax_check
# =============================================================================
# Validate Silver source files without generating code.
#
# Usage:
#   silver_syntax_check(<target> <source>...
#       [INCLUDE_DIRS <dir>...]
#       [DEFINES <def>...]
#   )
# =============================================================================
function(silver_syntax_check TARGET_NAME)
    cmake_parse_arguments(PARSE_ARGV 1 SILVER
        ""
        ""
        "SOURCES;INCLUDE_DIRS;DEFINES"
    )

    set(SOURCE_FILES ${SILVER_UNPARSED_ARGUMENTS} ${SILVER_SOURCES})

    if(NOT SOURCE_FILES)
        message(FATAL_ERROR "silver_syntax_check: No source files specified for target ${TARGET_NAME}")
    endif()

    # Compiler target/executable
    if(DEFINED SILVER_COMPILER)
        set(AGC_CMD "${SILVER_COMPILER}")
        set(AGC_DEP "")
    else()
        set(AGC_TARGET agc)
        set(AGC_CMD "$<TARGET_FILE:${AGC_TARGET}>")
        set(AGC_DEP ${AGC_TARGET})
    endif()

    set(STAMP_FILE "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.syntax_check.stamp")

    set(AGC_ARGS "")

    # Add all source files
    foreach(SOURCE_FILE ${SOURCE_FILES})
        get_filename_component(SOURCE_ABS "${SOURCE_FILE}" ABSOLUTE)
        list(APPEND AGC_ARGS "${SOURCE_ABS}")
    endforeach()

    # Syntax check only
    list(APPEND AGC_ARGS "-fsyntax-only")

    # Include directories
    list(APPEND AGC_ARGS "-I" "${SILVER_LIBAG_INCLUDE_DIR}")
    foreach(dir ${SILVER_DEFAULT_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()
    foreach(dir ${SILVER_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()

    # Preprocessor definitions
    foreach(def ${SILVER_DEFAULT_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()
    foreach(def ${SILVER_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()

    add_custom_command(
        OUTPUT ${STAMP_FILE}
        COMMAND ${AGC_CMD} ${AGC_ARGS}
        COMMAND ${CMAKE_COMMAND} -E touch ${STAMP_FILE}
        DEPENDS ${AGC_DEP} ${SOURCE_FILES}
        COMMENT "Checking Silver syntax: ${TARGET_NAME}"
        VERBATIM
    )

    add_custom_target(${TARGET_NAME} DEPENDS ${STAMP_FILE})
endfunction()

# =============================================================================
# silver_ast_dump
# =============================================================================
# Generate AST dump output for debugging/analysis.
#
# Usage:
#   silver_ast_dump(<target> <source>
#       [OUTPUT_FILE <file>]
#       [INCLUDE_DIRS <dir>...]
#       [DEFINES <def>...]
#   )
# =============================================================================
function(silver_ast_dump TARGET_NAME SOURCE_FILE)
    cmake_parse_arguments(PARSE_ARGV 2 SILVER
        ""
        "OUTPUT_FILE"
        "INCLUDE_DIRS;DEFINES"
    )

    get_filename_component(SOURCE_ABS "${SOURCE_FILE}" ABSOLUTE)
    get_filename_component(SOURCE_NAME "${SOURCE_FILE}" NAME_WE)

    # Output file
    if(SILVER_OUTPUT_FILE)
        set(OUTPUT_FILE "${SILVER_OUTPUT_FILE}")
    else()
        set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${SOURCE_NAME}.ast")
    endif()

    # Compiler target/executable
    if(DEFINED SILVER_COMPILER)
        set(AGC_CMD "${SILVER_COMPILER}")
        set(AGC_DEP "")
    else()
        set(AGC_TARGET agc)
        set(AGC_CMD "$<TARGET_FILE:${AGC_TARGET}>")
        set(AGC_DEP ${AGC_TARGET})
    endif()

    set(AGC_ARGS "")
    list(APPEND AGC_ARGS "${SOURCE_ABS}")
    list(APPEND AGC_ARGS "-ast-dump")

    # Include directories
    list(APPEND AGC_ARGS "-I" "${SILVER_LIBAG_INCLUDE_DIR}")
    foreach(dir ${SILVER_DEFAULT_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()
    foreach(dir ${SILVER_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()

    # Preprocessor definitions
    foreach(def ${SILVER_DEFAULT_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()
    foreach(def ${SILVER_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()

    add_custom_command(
        OUTPUT ${OUTPUT_FILE}
        COMMAND ${AGC_CMD} ${AGC_ARGS} > ${OUTPUT_FILE}
        DEPENDS ${AGC_DEP} ${SOURCE_ABS}
        COMMENT "Generating AST dump: ${SOURCE_NAME}.ast"
        VERBATIM
    )

    add_custom_target(${TARGET_NAME} DEPENDS ${OUTPUT_FILE})

    set_target_properties(${TARGET_NAME} PROPERTIES
        SILVER_AST_FILE "${OUTPUT_FILE}"
    )
endfunction()

# =============================================================================
# silver_emit_ir
# =============================================================================
# Generate intermediate representation output for analysis.
#
# Usage:
#   silver_emit_ir(<target> <source>
#       [OUTPUT_FILE <file>]
#       [BACKEND <backend>]
#       [INCLUDE_DIRS <dir>...]
#       [DEFINES <def>...]
#   )
# =============================================================================
function(silver_emit_ir TARGET_NAME SOURCE_FILE)
    cmake_parse_arguments(PARSE_ARGV 2 SILVER
        ""
        "OUTPUT_FILE;BACKEND"
        "INCLUDE_DIRS;DEFINES"
    )

    get_filename_component(SOURCE_ABS "${SOURCE_FILE}" ABSOLUTE)
    get_filename_component(SOURCE_NAME "${SOURCE_FILE}" NAME_WE)

    # Determine extension based on backend
    if(SILVER_BACKEND STREQUAL "js")
        set(IR_EXT ".js")
    elseif(SILVER_BACKEND STREQUAL "csharp")
        set(IR_EXT ".cs")
    else()
        set(IR_EXT ".ll")
    endif()

    # Output file
    if(SILVER_OUTPUT_FILE)
        set(OUTPUT_FILE "${SILVER_OUTPUT_FILE}")
    else()
        set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${SOURCE_NAME}${IR_EXT}")
    endif()

    # Compiler target/executable
    if(DEFINED SILVER_COMPILER)
        set(AGC_CMD "${SILVER_COMPILER}")
        set(AGC_DEP "")
    else()
        set(AGC_TARGET agc)
        set(AGC_CMD "$<TARGET_FILE:${AGC_TARGET}>")
        set(AGC_DEP ${AGC_TARGET})
    endif()

    set(AGC_ARGS "")
    list(APPEND AGC_ARGS "${SOURCE_ABS}")
    list(APPEND AGC_ARGS "-S")
    list(APPEND AGC_ARGS "-o" "${OUTPUT_FILE}")

    # Backend selection
    if(SILVER_BACKEND)
        list(APPEND AGC_ARGS "--emit=${SILVER_BACKEND}")
    elseif(SILVER_DEFAULT_BACKEND)
        list(APPEND AGC_ARGS "--emit=${SILVER_DEFAULT_BACKEND}")
    endif()

    # Include directories
    list(APPEND AGC_ARGS "-I" "${SILVER_LIBAG_INCLUDE_DIR}")
    foreach(dir ${SILVER_DEFAULT_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()
    foreach(dir ${SILVER_INCLUDE_DIRS})
        list(APPEND AGC_ARGS "-I" "${dir}")
    endforeach()

    # Preprocessor definitions
    foreach(def ${SILVER_DEFAULT_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()
    foreach(def ${SILVER_DEFINES})
        list(APPEND AGC_ARGS "-D" "${def}")
    endforeach()

    add_custom_command(
        OUTPUT ${OUTPUT_FILE}
        COMMAND ${AGC_CMD} ${AGC_ARGS}
        DEPENDS ${AGC_DEP} ${SOURCE_ABS}
        COMMENT "Generating IR: ${SOURCE_NAME}${IR_EXT}"
        VERBATIM
    )

    add_custom_target(${TARGET_NAME} DEPENDS ${OUTPUT_FILE})

    set_target_properties(${TARGET_NAME} PROPERTIES
        SILVER_IR_FILE "${OUTPUT_FILE}"
    )
endfunction()

# =============================================================================
# silver_add_test
# =============================================================================
# Add a Silver program as a CTest test.
#
# Usage:
#   silver_add_test(<name> <source>
#       [WORKING_DIRECTORY <dir>]
#       [ARGS <arg>...]
#       [EXPECTED_RETURN <code>]
#       [LABELS <label>...]
#       [INCLUDE_DIRS <dir>...]
#       [LINK_DIRS <dir>...]
#       [LINK_LIBS <lib>...]
#       [DEFINES <def>...]
#   )
# =============================================================================
function(silver_add_test TEST_NAME SOURCE_FILE)
    cmake_parse_arguments(PARSE_ARGV 2 SILVER
        ""
        "WORKING_DIRECTORY;EXPECTED_RETURN"
        "ARGS;LABELS;INCLUDE_DIRS;LINK_DIRS;LINK_LIBS;DEFINES"
    )

    # Create the executable target
    set(TARGET_NAME "test_${TEST_NAME}")
    add_silver_executable(${TARGET_NAME} ${SOURCE_FILE}
        INCLUDE_DIRS ${SILVER_INCLUDE_DIRS}
        LINK_DIRS ${SILVER_LINK_DIRS}
        LINK_LIBS ${SILVER_LINK_LIBS}
        DEFINES ${SILVER_DEFINES}
    )

    # Get output file path
    set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}")

    # Add the test
    if(SILVER_WORKING_DIRECTORY)
        add_test(
            NAME ${TEST_NAME}
            COMMAND ${OUTPUT_FILE} ${SILVER_ARGS}
            WORKING_DIRECTORY ${SILVER_WORKING_DIRECTORY}
        )
    else()
        add_test(
            NAME ${TEST_NAME}
            COMMAND ${OUTPUT_FILE} ${SILVER_ARGS}
        )
    endif()

    # Set expected return code
    if(DEFINED SILVER_EXPECTED_RETURN)
        set_tests_properties(${TEST_NAME} PROPERTIES
            PASS_REGULAR_EXPRESSION ""
            FAIL_REGULAR_EXPRESSION ""
        )
        set_tests_properties(${TEST_NAME} PROPERTIES
            WILL_FAIL $<NOT:$<EQUAL:${SILVER_EXPECTED_RETURN},0>>
        )
    endif()

    # Add labels
    if(SILVER_LABELS)
        set_tests_properties(${TEST_NAME} PROPERTIES
            LABELS "${SILVER_LABELS}"
        )
    endif()

    # Make test depend on the executable
    set_tests_properties(${TEST_NAME} PROPERTIES
        DEPENDS ${TARGET_NAME}
    )
endfunction()

# =============================================================================
# silver_glob_executables
# =============================================================================
# Create executable targets for all .ag files matching a pattern.
#
# Usage:
#   silver_glob_executables(
#       PATTERN <glob>
#       [PREFIX <prefix>]
#       [EXCLUDE <pattern>...]
#       [INCLUDE_DIRS <dir>...]
#       [LINK_DIRS <dir>...]
#       [LINK_LIBS <lib>...]
#       [DEFINES <def>...]
#       [STATIC]
#       [VERBOSE]
#       [OUTPUT_TARGETS <var>]
#   )
# =============================================================================
function(silver_glob_executables)
    cmake_parse_arguments(PARSE_ARGV 0 SILVER
        "STATIC;VERBOSE"
        "PATTERN;PREFIX;OUTPUT_TARGETS"
        "EXCLUDE;INCLUDE_DIRS;LINK_DIRS;LINK_LIBS;DEFINES"
    )

    if(NOT SILVER_PATTERN)
        message(FATAL_ERROR "silver_glob_executables: PATTERN is required")
    endif()

    file(GLOB SOURCE_FILES ${SILVER_PATTERN})

    set(CREATED_TARGETS "")

    foreach(SOURCE_FILE ${SOURCE_FILES})
        get_filename_component(SOURCE_NAME ${SOURCE_FILE} NAME_WE)

        # Check exclusions
        set(EXCLUDED FALSE)
        foreach(excl ${SILVER_EXCLUDE})
            if(SOURCE_NAME MATCHES "${excl}")
                set(EXCLUDED TRUE)
                break()
            endif()
        endforeach()

        if(EXCLUDED)
            message(STATUS "Skipping excluded Silver file: ${SOURCE_NAME}")
            continue()
        endif()

        # Build target name
        if(SILVER_PREFIX)
            set(TARGET_NAME "${SILVER_PREFIX}${SOURCE_NAME}")
        else()
            set(TARGET_NAME "${SOURCE_NAME}")
        endif()

        # Create the executable
        set(EXTRA_OPTS "")
        if(SILVER_STATIC)
            list(APPEND EXTRA_OPTS STATIC)
        endif()
        if(SILVER_VERBOSE)
            list(APPEND EXTRA_OPTS VERBOSE)
        endif()

        add_silver_executable(${TARGET_NAME} ${SOURCE_FILE}
            INCLUDE_DIRS ${SILVER_INCLUDE_DIRS}
            LINK_DIRS ${SILVER_LINK_DIRS}
            LINK_LIBS ${SILVER_LINK_LIBS}
            DEFINES ${SILVER_DEFINES}
            ${EXTRA_OPTS}
        )

        list(APPEND CREATED_TARGETS ${TARGET_NAME})
    endforeach()

    # Output list of created targets
    if(SILVER_OUTPUT_TARGETS)
        set(${SILVER_OUTPUT_TARGETS} ${CREATED_TARGETS} PARENT_SCOPE)
    endif()
endfunction()

# =============================================================================
# silver_find_compiler
# =============================================================================
# Find the Silver compiler executable.
#
# Usage:
#   silver_find_compiler([REQUIRED] [HINTS <path>...])
#
# Sets:
#   SILVER_COMPILER - Path to agc executable
#   SILVER_COMPILER_FOUND - TRUE if found
# =============================================================================
function(silver_find_compiler)
    cmake_parse_arguments(PARSE_ARGV 0 SILVER
        "REQUIRED"
        ""
        "HINTS"
    )

    # If agc target exists, use it
    if(TARGET agc)
        set(SILVER_COMPILER "$<TARGET_FILE:agc>" PARENT_SCOPE)
        set(SILVER_COMPILER_FOUND TRUE PARENT_SCOPE)
        return()
    endif()

    # Search for installed compiler
    find_program(SILVER_COMPILER_PATH agc
        HINTS ${SILVER_HINTS}
        PATHS
            /usr/local/bin
            /usr/bin
            ${CMAKE_SOURCE_DIR}/bootstrap/bin
    )

    if(SILVER_COMPILER_PATH)
        set(SILVER_COMPILER "${SILVER_COMPILER_PATH}" PARENT_SCOPE)
        set(SILVER_COMPILER_FOUND TRUE PARENT_SCOPE)
        message(STATUS "Found Silver compiler: ${SILVER_COMPILER_PATH}")
    else()
        set(SILVER_COMPILER_FOUND FALSE PARENT_SCOPE)
        if(SILVER_REQUIRED)
            message(FATAL_ERROR "Silver compiler (agc) not found")
        else()
            message(STATUS "Silver compiler not found")
        endif()
    endif()
endfunction()

# =============================================================================
# Utility: Print Silver configuration
# =============================================================================
function(silver_print_config)
    message(STATUS "=== Silver CMake Configuration ===")
    message(STATUS "  SILVER_LIBAG_DIR:           ${SILVER_LIBAG_DIR}")
    message(STATUS "  SILVER_LIBAG_INCLUDE_DIR:   ${SILVER_LIBAG_INCLUDE_DIR}")
    message(STATUS "  SILVER_DEFAULT_INCLUDE_DIRS: ${SILVER_DEFAULT_INCLUDE_DIRS}")
    message(STATUS "  SILVER_DEFAULT_LINK_DIRS:   ${SILVER_DEFAULT_LINK_DIRS}")
    message(STATUS "  SILVER_DEFAULT_LINK_LIBS:   ${SILVER_DEFAULT_LINK_LIBS}")
    message(STATUS "  SILVER_DEFAULT_DEFINES:     ${SILVER_DEFAULT_DEFINES}")
    message(STATUS "  SILVER_DEFAULT_BACKEND:     ${SILVER_DEFAULT_BACKEND}")
    message(STATUS "  SILVER_VERBOSE:             ${SILVER_VERBOSE}")
    message(STATUS "  SILVER_STATIC_LINK:         ${SILVER_STATIC_LINK}")
    if(DEFINED SILVER_COMPILER)
        message(STATUS "  SILVER_COMPILER:            ${SILVER_COMPILER}")
    else()
        message(STATUS "  SILVER_COMPILER:            <using agc target>")
    endif()
    message(STATUS "==================================")
endfunction()