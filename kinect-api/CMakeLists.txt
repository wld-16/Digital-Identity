project(kinect-api)

cmake_minimum_required(VERSION 2.8)

set(GLUT_ROOT_PATH "D:\\Libs\\freeglut-3.2.1")
set(FREEGLUT_LIBRARY "${GLUT_ROOT_PATH}\\build2013win32\\lib\\Debug\\freeglutd.lib")
set(FREEGLUT_INCLUDE_DIR "${GLUT_ROOT_PATH}\\include")

set(GLEW_ROOT_DIR "D:\\Libs\\glew-cmake-glew-cmake-2.1.0")
set(GLEW_INCLUDE_DIR "${GLEW_ROOT_DIR}\\include")
set(GLEW_LIBRARIES "${GLEW_ROOT_DIR}\\build2013win32\\Lib\\Debug\\")
set(GLEW_SHARED_LIBRARY_DEBUG "${GLEW_LIBRARIES}\\libglew_sharedd.lib")

set(JSON "D:\\Libs\\jsoncpp-1.6.0")
set(JSON_INCLUDE_DIR ${JSON}\\include)
set(JSON_LIBRARY ${JSON}\\build2013win32\\src\\lib_json\\Debug\\jsoncpp.lib)

set(WSPP_INCLUDE_DIR "D:\\Libs\\websocketpp-0.8.2")

set(BOOST_ROOT "D:\\Libs\\boost_1_55_0")
set(BOOST_INCLUDE_DIRS "${BOOST_ROOT}")
set(BOOST_LIBRARY_DIR "${BOOST_ROOT}\\stage\\lib")

set(KinectToolkit_FACE TRUE)

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
        ${FREEGLUT_INCLUDE_DIR}
        ${GLEW_INCLUDE_DIR}
        ${JSON_INCLUDE_DIR}
        ${WSPP_INCLUDE_DIR}
        ${BOOST_INCLUDE_DIRS}
)

add_executable(kinect-api
        src/main.cpp
        src/main.h
        src/glut.h
        src/glut.cpp
        src/kinect_websocket.cpp
        src/kinect_websocket.h
        src/kinect_processor.cpp
        src/kinect_processor.h
        src/faceTracking/FTHelper.cpp
        src/faceTracking/FTHelper.h
        src/faceTracking/KinectSensor.cpp
        src/faceTracking/KinectSensor.h
        )

target_link_libraries(kinect-api
        ${JSON_LIBRARY}
        ${KinectToolkit_LIBRARIES}
        ${KinectSDK_LIBRARIES}
        ${FREEGLUT_LIBRARY}
        ${OPENGL_LIBRARIES}
        ${GLEW_SHARED_LIBRARY_DEBUG}
        ${BOOST_LIBRARY_DIR}\\libboost_thread-vc120-mt-gd-1_55.lib
        ${BOOST_LIBRARY_DIR}\\libboost_date_time-vc120-mt-gd-1_55.lib
        ${BOOST_LIBRARY_DIR}\\libboost_system-vc120-mt-gd-1_55.lib
        ${BOOST_LIBRARY_DIR}\\libboost_chrono-vc120-mt-gd-1_55.lib
        ${BOOST_LIBRARY_DIR}\\libboost_regex-vc120-mt-gd-1_55.lib

)
