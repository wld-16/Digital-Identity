//
// Created by wnabo on 18.11.2020.
//

#include <Ole2.h>
#include <NuiApi.h>
#include <NuiImageCamera.h>
#include <NuiSensor.h>
#include <array>

void getKinectData();
std::array<Vector4, NUI_SKELETON_POSITION_COUNT> *getSkeletonPosition();
void fillKinectIntoJson();
bool initKinectSkeletonTracking();
bool initKinectFaceTracking();
void getKinectHeadData();
