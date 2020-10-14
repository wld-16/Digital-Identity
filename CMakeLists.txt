cmake_minimum_required(VERSION 3.13)
project(Digital_Identity)

set(CMAKE_CXX_STANDARD 17)

# See https://gist.github.com/UnaNancyOwen/b7bb7b895156a4ed3e6e for FindKinectSDK
set(CMAKE_MODULE_PATH "C:\\Program Files\\CMake\\share\\cmake-3.13\\Modules")
set(GLUT_INCLUDE_DIR "D:\\Libs\\freeglut-3.2.1\\include")
set(GLUT_LIBRARY_DIR "D:\\Libs\\freeglut-3.2.1\\build\\lib\\Debug")

find_package(OpenGL REQUIRED)
find_package(GLUT REQUIRED)
find_package(KinectSDK REQUIRED)

include_directories(${GLUT_INCLUDE_DIR} ${KinectSDK_INCLUDE_DIRS} ${KinectToolkit_INCLUDE_DIRS})
add_executable(Digital_Identity main.cpp)

message("${GLUT_LIBRARY}")

target_link_libraries(Digital_Identity ${OPENGL_LIBRARIES} ${GLUT_LIBRARY} ${KinectSDK_LIBRARIES} ${KinectToolkit_LIBRARIES})
