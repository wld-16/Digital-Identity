project(kinect-api)

cmake_minimum_required(VERSION 2.8)

set(ENV{KINECT_TOOLKIT_DIR} "C:\\Program Files\\Microsoft SDKs\\Kinect\\Developer Toolkit v1.8.0")
set(ENV{KINECTSDK10_DIR} "C:\\Program Files\\Microsoft SDKs\\Kinect\\v1.8")
set(GLUT_ROOT_PATH "D:\\Libs\\freeglut-3.2.1")
set(FREEGLUT_LIBRARY "${GLUT_ROOT_PATH}\\build2013win32\\lib\\Debug\\freeglutd.lib")
set(FREEGLUT_INCLUDE_DIR "${GLUT_ROOT_PATH}\\include")

set(GLEW_ROOT_DIR "D:\\Libs\\glew-cmake-glew-cmake-2.1.0")
set(GLEW_INCLUDE_DIR "${GLEW_ROOT_DIR}\\include")
set(GLEW_LIBRARIES "${GLEW_ROOT_DIR}\\build2013win32\\Lib\\Debug\\")
set(GLEW_SHARED_LIBRARY_DEBUG "${GLEW_LIBRARIES}\\libglew_sharedd.lib")

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS /ENTRY:main")

find_package(OpenGL REQUIRED)
find_package(KinectSDK REQUIRED)

IF ( NOT GLUT_FOUND )
    IF ( MSVC )
        SET ( GLUT_FOUND 1 )
        SET ( GLUT_INCLUDE_DIR ${FREEGLUT_INCLUDE_DIR} )
        SET ( GLUT_LIBRARIES ${FREEGLUT_LIBRARY} )
    ELSE ( MSVC )
        MESSAGE( WARNING "Please install glut." )
    ENDIF ( MSVC )
ENDIF ( NOT GLUT_FOUND )

include_directories(
        ${KinectSDK_INCLUDE_DIRS}
        ${KinectToolkit_INCLUDE_DIRS}
        ${OpenCV_INCLUDE_DIRS}
        ${FREEGLUT_INCLUDE_DIR}
        ${GLEW_INCLUDE_DIR}
)

add_executable(kinect-api
        src/main.cpp
        src/main.h
        src/glut.h
        src/glut.cpp)

target_link_libraries(kinect-api
        ${KinectToolkit_LIBRARIES}
        ${KinectSDK_LIBRARIES}
        ${OpenCV_LIBRARIES}
        ${FREEGLUT_LIBRARY}
        ${OPENGL_LIBRARIES}
        ${GLEW_SHARED_LIBRARY_DEBUG}
)
