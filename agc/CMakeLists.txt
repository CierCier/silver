cmake_minimum_required(VERSION 3.10)

# Backend options - LLVM is the default/primary backend
option(SILVER_ENABLE_LLVM "Enable LLVM backend (default)" ON)
option(SILVER_ENABLE_JS "Enable JavaScript backend" OFF)
option(SILVER_ENABLE_CSHARP "Enable C# backend" OFF)

# Create a library for the compiler core so tests can link to it
set(AGC_LIB_SOURCES
	src/lexer.cpp
	src/parser.cpp
	src/ast_dump.cpp
	src/codegen_registry.cpp
	src/comptime.cpp
    src/sema.cpp
)

# LLVM backend (default)
if(SILVER_ENABLE_LLVM)
	find_package(LLVM REQUIRED CONFIG)
	message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
	message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
	list(APPEND AGC_LIB_SOURCES src/backends/codegen_llvm.cpp)
endif()

# Optional JS backend
if(SILVER_ENABLE_JS)
	list(APPEND AGC_LIB_SOURCES src/backends/codegen_js.cpp)
endif()

# Optional C# backend
if(SILVER_ENABLE_CSHARP)
	list(APPEND AGC_LIB_SOURCES src/backends/codegen_csharp.cpp)
endif()

add_library(agc_lib STATIC ${AGC_LIB_SOURCES})

target_include_directories(agc_lib PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${CMAKE_CURRENT_SOURCE_DIR}/../libag/include
	${CMAKE_CURRENT_BINARY_DIR}/include
)

if(SILVER_ENABLE_LLVM)
	target_include_directories(agc_lib PUBLIC ${LLVM_INCLUDE_DIRS})
	target_compile_definitions(agc_lib PUBLIC SILVER_HAS_LLVM=1)
	# Use LLVM shared library if available, otherwise use component libs
	if(TARGET LLVM)
		target_link_libraries(agc_lib PUBLIC LLVM)
	else()
		llvm_map_components_to_libnames(llvm_libs support core irreader)
		target_link_libraries(agc_lib PUBLIC ${llvm_libs})
	endif()
endif()

if(SILVER_ENABLE_JS)
	target_compile_definitions(agc_lib PUBLIC SILVER_HAS_JS=1)
endif()

if(SILVER_ENABLE_CSHARP)
	target_compile_definitions(agc_lib PUBLIC SILVER_HAS_CSHARP=1)
endif()

# Link libag to agc_lib
if (TARGET ag_static)
	target_link_libraries(agc_lib PUBLIC ag_static)
elseif(TARGET ag_shared)
	target_link_libraries(agc_lib PUBLIC ag_shared)
endif()

# The main executable links to the library
add_executable(agc src/main.cpp)
target_link_libraries(agc PRIVATE agc_lib)

# Add tests
enable_testing()
add_subdirectory(tests)
