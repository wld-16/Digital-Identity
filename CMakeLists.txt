cmake_minimum_required(VERSION 3.16)
project(fashion_ar)

SET(OpenCV_DIR /home/spacewave/.local/lib/opencv-4.4.0/build)
find_package(OpenCV REQUIRED)
include_directories(${OpenCV_INCLUDE_DIRS})

set(CMAKE_CXX_STANDARD 20)

add_executable(fashion_ar main.cpp)
target_link_libraries(fashion_ar ${OpenCV_LIBS})