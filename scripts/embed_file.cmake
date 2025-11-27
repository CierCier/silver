# embed_file.cmake
file(READ ${INPUT_FILE} CONTENT)
# Escape quotes and backslashes
string(REPLACE "\\" "\\\\" CONTENT "${CONTENT}")
string(REPLACE "\"" "\\\"" CONTENT "${CONTENT}")
string(REPLACE "\n" "\\n\"\n\"" CONTENT "${CONTENT}")
file(WRITE ${OUTPUT_FILE} "namespace agc { constexpr const char* crt0_source = \"${CONTENT}\"; }")
