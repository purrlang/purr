cmake_minimum_required(VERSION 3.10)
project(purr_runtime C)

# Create the purr_runtime library
add_library(purr_runtime STATIC
    purr_runtime.c
)

# Set C standard
set_target_properties(purr_runtime PROPERTIES
    C_STANDARD 99
    C_STANDARD_REQUIRED ON
)

# Add the runtime/include directory as public include directory
target_include_directories(purr_runtime PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Export the library for external use
set(PURR_RUNTIME_LIB purr_runtime PARENT_SCOPE)
set(PURR_RUNTIME_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE)
