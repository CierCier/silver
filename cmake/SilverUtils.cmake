function(add_silver_executable TARGET_NAME SOURCE_FILE)
    # Ensure the output directory exists
    set(OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}")
    
    # We depend on the compiler and the runtime library
    # Assuming 'agc' and 'ag_shared' are targets defined in the main project
    set(AGC_TARGET agc)
    set(LIBAG_TARGET ag_shared)

    # Pass the library path to the compiler
    # The library is built in the libag subdirectory of the build tree
    set(LIBAG_DIR "${CMAKE_BINARY_DIR}/libag")
    set(LIBAG_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/libag/include")

    add_custom_command(
        OUTPUT ${OUTPUT_FILE}
        COMMAND $<TARGET_FILE:${AGC_TARGET}>
        ARGS ${SOURCE_FILE} -o ${OUTPUT_FILE} -L ${LIBAG_DIR} -I ${LIBAG_INCLUDE_DIR}
        DEPENDS ${AGC_TARGET} ${LIBAG_TARGET} ${SOURCE_FILE}
        COMMENT "Compiling Silver program ${TARGET_NAME}"
        VERBATIM
    )

    add_custom_target(${TARGET_NAME} ALL DEPENDS ${OUTPUT_FILE})
endfunction()
