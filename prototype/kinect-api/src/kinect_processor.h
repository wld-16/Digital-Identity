//
// Created by wnabo on 18.11.2020.
//

#include <Ole2.h>
#include <NuiApi.h>
#include <NuiImageCamera.h>
#include <NuiSensor.h>
#include <array>
#include <GL/glew.h>

void getKinectData(GLuint &vboId, GLuint& cboId);
std::array<Vector4, NUI_SKELETON_POSITION_COUNT> *getSkeletonPosition();
void fillKinectIntoJson();
bool initKinectSkeletonTracking();
void run();
BOOL getHeadColorImage(GLubyte *dest);
bool getSkeletonFound();
std::array<Vector4, NUI_SKELETON_POSITION_COUNT> *getBonesEulerAngles();
