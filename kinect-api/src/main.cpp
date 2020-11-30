#include "glut.h"
#include "kinect_websocket.h"
#include "kinect_processor.h"
#include "json_file_io.h"
#include <thread>

void runWebserver(){
    kinect_websocket kinectWebsocket;
    std::string *json = getJson();
    kinectWebsocket.setJson(json);
    kinectWebsocket.run();
}

void runFileWrite(){
    json_file_io jsonFileIo;
    std::string *json = getJson();
    jsonFileIo.setJson(json);
    jsonFileIo.setWritePath("../../test.json");
    jsonFileIo.run();
}



int main(int argc, char *argv[]) {
    //if (!initHead(argc, argv)) return 1;
    if (!init(argc, argv)) return 1;

    //if (!initKinectFaceTracking()) return 1;
    if (!initKinectSkeletonTracking()) return 1;

    thread websocketThread(runWebserver);
    //thread fileIOThread(runFileWrite);

    initGL();

    // Main loop
    execute();
    return 0;
}