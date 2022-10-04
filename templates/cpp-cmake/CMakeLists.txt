cmake_minimum_required(VERSION __(cmake-version)__)
project(__PROJECT-NAME__)

set(TARGET_NAME ${PROJECT_NAME})

file(GLOB TARGET_SRCS CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/src/*.cpp")

add_executable(${TARGET_NAME} ${TARGET_SRCS})
target_include_directories(${TARGET_NAME} PRIVATE "${PROJECT_SOURCE_DIR}/src/include")
