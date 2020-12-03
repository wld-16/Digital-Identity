cmake_minimum_required(VERSION 3.14)
project(Digital_Identity)

set(CMAKE_CXX_STANDARD 17)

# See https://gist.github.com/UnaNancyOwen/b7bb7b895156a4ed3e6e for FindKinectSDK
set(CMAKE_MODULE_PATH "C:\\Program Files\\CMake\\share\\cmake-3.13\\Modules")

set(OpenCV_DIR "D:\\Libs\\opencv-4.5.0\\build2019win32")

set(GLUT_ROOT_PATH "D:\\Libs\\freeglut-3.2.1")
set(FREEGLUT_LIBRARY "${GLUT_ROOT_PATH}\\build2019win32\\lib\\Debug\\freeglutd.lib")
set(FREEGLUT_INCLUDE_DIR "${GLUT_ROOT_PATH}\\include")

set(GLEW_ROOT_DIR "D:\\Libs\\glew-cmake-glew-cmake-2.1.0")
set(GLEW_INCLUDE_DIR "${GLEW_ROOT_DIR}\\include")
set(GLEW_LIBRARIES "${GLEW_ROOT_DIR}\\build2019win32\\Lib\\Debug\\")
set(GLEW_SHARED_LIBRARY_DEBUG "${GLEW_LIBRARIES}\\libglew_sharedd.lib")

set(ASSIMP_ROOT "D:\\Libs\\assimp-5.0.1")
set(ASSIMP_INCLUDE_DIR "${ASSIMP_ROOT}\\include")
set(ASSIMP_LIBRARY "${ASSIMP_ROOT}\\build2019\\code\\Debug\\assimp-vc142-mtd.lib")

set(OGL_ROOT_DIR D:\\Dev\\Tutorials\\ogldev-source)
set(OGL_INCLUDE_DIR ${OGL_ROOT_DIR}\\Include)
set(OGL_LIBRARY ${OGL_ROOT_DIR}\\build2019win32\\Debug\\ogl-tutorials.lib)

set(ENV{KINECT_TOOLKIT_DIR} "C:\\Program Files\\Microsoft SDKs\\Kinect\\Developer Toolkit v1.8.0")
set(ENV{KINECTSDK10_DIR} "C:\\Program Files\\Microsoft SDKs\\Kinect\\v1.8")

set(FT_ROOT "D:\\Libs\\freetype-windows-binaries-2.10.4")
set(FT_LIBRARY "${FT_ROOT}\\win32\\freetype.lib")
set(FT_INCLUDE_DIR "${FT_ROOT}\\include")

set(BOOST_ROOT "D:\\Libs\\boost_1_74_0")
set(BOOST_INCLUDE_DIRS "${BOOST_ROOT}")
set(BOOST_LIBRARY_DIRS "${BOOST_ROOT}\\stage\\lib")

set(WEBSOCKETS_INCLUDE_DIR "D:\\Libs\\websocketpp-0.8.2")

find_package(KinectSDK REQUIRED)

include_directories(${KinectSDK_INCLUDE_DIRS} ${KinectToolkit_INCLUDE_DIRS} ${JSON_PARSER_INCLUDE_DIR} ${WEBSOCKETS_INCLUDE_DIR} ${JSON_INCLUDE} ${BOOST_INCLUDE_DIRS})

add_subdirectory(${OGL_ROOT_DIR} ${OGL_ROOT_DIR}\\build2019win32\\Debug)

find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)
find_package(OpenCV REQUIRED)
find_package(GLEW REQUIRED)

if(NOT GLEW_FOUND)
    message(WARNING "GLEW NOT found, can't build FGPanel")
    set(WITH_FGPANEL 0)
    return()
endif()

message("${OpenGL_VERSION}")

if(NOT GLEW_LIBRARIES)
    if(GLEW_LIBRARY_DEBUG AND GLEW_LIBRARY_RELEASE)
        set(GLEW_LIBRARIES "optimized;${GLEW_LIBRARY_RELEASE};debug;${GLEW_LIBRARY_DEBUG}")
    else()
        set(GLEW_LIBRARIES GLEW::GLEW)
    endif()
endif()

IF ( NOT GLUT_FOUND )
    IF ( MSVC )
        SET ( GLUT_FOUND 1 )
        SET ( GLUT_INCLUDE_DIR ${FREEGLUT_INCLUDE_DIR} )
        SET ( GLUT_LIBRARIES ${FREEGLUT_LIBRARY} )
    ELSE ( MSVC )
        MESSAGE( WARNING "Please install glut." )
    ENDIF ( MSVC )
ENDIF ( NOT GLUT_FOUND )

if ( MSVC )
    ADD_DEFINITIONS( -D_SCL_SECURE_NO_WARNINGS )
    ADD_DEFINITIONS( -D_CRT_SECURE_NO_WARNINGS )
endif ( MSVC )

include_directories(
        ${OpenCV_INCLUDE_DIRS}
        ${GLEW_INCLUDE_DIR}
        ${GLUT_INCLUDE_DIR}
        ${OPENGL_INCLUDE_DIR}
        ${ASSIMP_INCLUDE_DIR}
        ${OGL_INCLUDE_DIR}
        ${FT_INCLUDE_DIR}
)

add_executable(Digital_Identity
        src/main.cpp
        src/ogl/mesh.cpp
        src/ogl/mesh.h
        src/ogl/glut_backend.cpp
        src/ogl/opencv_texture.cpp
        src/ogl/opencv_texture.h
        src/ogl/ICallbacks.h
        src/ogl/glut_backend.h
        src/ogl/skinned_mesh.h
        src/ogl/shadow_map_fbo.h
        src/ogl/shadow_map_fbo.cpp
        src/ogl/shadow_map_technique.cpp
        src/ogl/shadow_map_technique.h
        src/ogl/skybox.cpp
        src/ogl/skybox.h
        src/ogl/skinning_technique.cpp
        src/ogl/skinning_technique.h
        src/ogl/skybox_technique.cpp
        src/ogl/skybox_technique.h
        src/ogl/cubemap_texture.cpp
        src/ogl/cubemap_texture.h
        src/ogl/opencv_skinned_mesh.cpp
        src/ogl/opencv_skinned_mesh.h
        src/kinect_data_client.cpp
        src/kinect_data_client.h src/json_file_reader.cpp src/json_file_reader.h src/Matf4f.h src/Quaternion.h src/ogl/font_technique.cpp src/ogl/font_technique.h)

target_link_libraries(
        Digital_Identity
        ${OpenCV_LIBRARIES}
        ${FREEGLUT_LIBRARY}
        ${OPENGL_LIBRARIES}
        ${ASSIMP_LIBRARY}
        ${GLEW_LIBRARIES}
        ${OGL_LIBRARY}
        ${GLEW_SHARED_LIBRARY_DEBUG}
        ${BOOST_LIBRARY_DIRS}\\libboost_random-vc142-mt-gd-x32-1_74.lib
        ${BOOST_LIBRARY_DIRS}\\libboost_date_time-vc142-mt-gd-x32-1_74.lib
        ${BOOST_LIBRARY_DIRS}\\libboost_regex-vc142-mt-gd-x32-1_74.lib
        ${BOOST_LIBRARY_DIRS}\\libboost_filesystem-vc142-mt-gd-x32-1_74.lib
        ${FT_LIBRARY}
)
