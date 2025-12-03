cmake_minimum_required(VERSION 3.10)

# Generate version.hpp from template
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/libag/version.hpp.in
    ${CMAKE_CURRENT_BINARY_DIR}/include/libag/version.hpp
    @ONLY
)

# Install the Silver standard library headers
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/libag
    DESTINATION include
    FILES_MATCHING PATTERN "*.ag"
)

# Install the generated version.hpp
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/libag/version.hpp
    DESTINATION include/libag
)

