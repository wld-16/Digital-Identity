//
// Created by wnabo on 18.11.2020.
//


#include <string>
#include "kinect_processor.h"
#include "main.h"
#include "glut.h"
#include "faceTracking/FTHelper.h"
#include <FaceTrackLib.h>
#include <boost/algorithm/string.hpp>

#include <json/json.h>
#include <GL/glew.h>
#include <array>
#include <list>
#include <iostream>
#include <fstream>


INuiSensor *sensor;
FTHelper m_FTHelper;
std::string jsonData;
IFTImage *m_pVideoBuffer;

typedef struct JointData {
    JointData(Vector4 position, Vector4 rotation, Matrix4 rotationMatrix) {
        this->position = position;
        this->rotation = rotation;
        this->rotationMatrix = rotationMatrix;
    }

    Vector4 position;
    Vector4 rotation;
    Matrix4 rotationMatrix;
};

bool writeToCSVFile = false;

typedef std::array<JointData, NUI_SKELETON_POSITION_COUNT> SkeletonData;

SkeletonData skeletonStructure = {
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4()),
        JointData(Vector4(), Vector4(), Matrix4())
};

// OpenGL Variables
long depthToRgbMap[width * height * 2];

int frame = 0;

// Stores the coordinates of each joint
std::array<Vector4, NUI_SKELETON_POSITION_COUNT> skeletonPosition;
std::list<Vector4> headData;

// Kinect variables
HANDLE depthStream;
HANDLE rgbStream;


void FTHelperCallingBack(PVOID pVoid) {
    std::cout << "Callback executed" << std::endl;
}


bool initKinectSkeletonTracking() {
    // Get a working kinect sensor
    int numSensors;
    if (NuiGetSensorCount(&numSensors) < 0 || numSensors < 1) return false;
    if (NuiCreateSensorByIndex(0, &sensor) < 0) return false;

    // Initialize sensor
    sensor->NuiInitialize(NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX | NUI_INITIALIZE_FLAG_USES_COLOR |
                          NUI_INITIALIZE_FLAG_USES_SKELETON);
    sensor->NuiImageStreamOpen(NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX, // Depth camera or rgb camera?
                               NUI_IMAGE_RESOLUTION_640x480,                // Image resolution
                               0,        // Image stream flags, e.g. near mode
                               2,        // Number of frames to buffer
                               NULL,     // Event handle
                               &depthStream);
    sensor->NuiImageStreamOpen(NUI_IMAGE_TYPE_COLOR, // Depth camera or rgb camera?
                               NUI_IMAGE_RESOLUTION_640x480,                // Image resolution
                               0,      // Image stream flags, e.g. near mode
                               2,      // Number of frames to buffer
                               NULL,   // Event handle
                               &rgbStream);
    sensor->NuiSkeletonTrackingEnable(NULL, 0); // NUI_SKELETON_TRACKING_FLAG_ENABLE_SEATED_SUPPORT for only upper body
    return sensor;
}

bool initKinectFaceTracking() {
    IFTFaceTracker *pFT = FTCreateFaceTracker();
    if (!pFT) {
        // Handle errors
    }
    // Video camera config with width, height, focal length in pixels
    // NUI_CAMERA_COLOR_NOMINAL_FOCAL_LENGTH_IN_PIXELS focal length is computed for 640x480 resolution
    // If you use different resolutions, multiply this focal length by the scaling factor
    FT_CAMERA_CONFIG videoCameraConfig = {640, 480, NUI_CAMERA_COLOR_NOMINAL_FOCAL_LENGTH_IN_PIXELS};

    // Depth camera config with width, height, focal length in pixels
    // NUI_CAMERA_COLOR_NOMINAL_FOCAL_LENGTH_IN_PIXELS focal length is computed for 320x240 resolution
    // If you use different resolutions, multiply this focal length by the scaling factor
    FT_CAMERA_CONFIG depthCameraConfig = {320, 240, NUI_CAMERA_DEPTH_NOMINAL_FOCAL_LENGTH_IN_PIXELS};

    // Initialize the face tracker
    HRESULT hr = pFT->Initialize(&videoCameraConfig, &depthCameraConfig, NULL, NULL);
    if (FAILED(hr)) {
        // Handle errors
    }
    IFTResult *pFTResult = NULL;
    hr = pFT->CreateFTResult(&pFTResult);
    if (FAILED(hr)) {
        // Handle errors
    }
    // Prepare image interfaces that hold RGB and depth data
    IFTImage *pColorFrame = FTCreateImage();
    IFTImage *pDepthFrame = FTCreateImage();
    if (!pColorFrame || !pDepthFrame) {
        // Handle errors
    }

    //
    m_FTHelper.Init(FTHelperCallingBack, nullptr, NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX,
                    NUI_IMAGE_RESOLUTION_320x240,
                    TRUE,
                    TRUE, // if near mode doesn't work, fall back to default mode
                    NUI_IMAGE_TYPE_COLOR,
                    NUI_IMAGE_RESOLUTION_640x480,
                    TRUE);
    return true;
}

void getSkeletalData() {
    NUI_SKELETON_FRAME skeletonFrame = {0};
    std::ofstream myfile;

    if (sensor->NuiSkeletonGetNextFrame(0, &skeletonFrame) >= 0) {
        sensor->NuiTransformSmooth(&skeletonFrame, NULL);


        // Loop over all sensed skeletons
        for (int z = 0; z < NUI_SKELETON_COUNT; ++z) {
            _NUI_SKELETON_BONE_ORIENTATION bones[ NUI_SKELETON_POSITION_COUNT ];
            const NUI_SKELETON_DATA &skeleton = skeletonFrame.SkeletonData[z];
            const NUI_SKELETON_DATA test = skeletonFrame.SkeletonData[z];
            // Check the state of the skeleton

            if (skeleton.eTrackingState == NUI_SKELETON_TRACKED) {

                NuiSkeletonCalculateBoneOrientations(&test, bones);


                // Copy the joint positions into our array
                for (int i = 0; i < NUI_SKELETON_POSITION_COUNT; ++i) {

                    skeletonPosition[i] = skeleton.SkeletonPositions[i];

                    if (skeleton.eSkeletonPositionTrackingState[i] == NUI_SKELETON_POSITION_NOT_TRACKED) {
                        skeletonPosition[i].w = 0;
                    }
                }
                for(size_t i = 0; i < NUI_SKELETON_POSITION_COUNT; i++){
                    skeletonStructure[i].position = skeletonPosition[i];
                    skeletonStructure[i].rotation = bones[i].absoluteRotation.rotationQuaternion;
                    skeletonStructure[i].rotationMatrix = bones[i].absoluteRotation.rotationMatrix;
                }
            }
        }
    }
}

void getDepthData(GLubyte *dest) {
    float *fdest = (float *) dest;
    long *depth2rgb = (long *) depthToRgbMap;
    NUI_IMAGE_FRAME imageFrame;
    NUI_LOCKED_RECT LockedRect;
    if (sensor->NuiImageStreamGetNextFrame(depthStream, 0, &imageFrame) < 0) return;
    INuiFrameTexture *texture = imageFrame.pFrameTexture;
    texture->LockRect(0, &LockedRect, NULL, 0);
    if (LockedRect.Pitch != 0) {
        const USHORT *curr = (const USHORT *) LockedRect.pBits;
        for (int j = 0; j < height; ++j) {
            for (int i = 0; i < width; ++i) {
                // Get depth of pixel in millimeters
                USHORT depth = NuiDepthPixelToDepth(*curr++);
                // Store coordinates of the point corresponding to this pixel
                Vector4 pos = NuiTransformDepthImageToSkeleton(i, j, depth << 3, NUI_IMAGE_RESOLUTION_640x480);
                *fdest++ = pos.x / pos.w;
                *fdest++ = pos.y / pos.w;
                *fdest++ = pos.z / pos.w;
                // Store the index into the color array corresponding to this pixel
                NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution(
                        NUI_IMAGE_RESOLUTION_640x480, NUI_IMAGE_RESOLUTION_640x480, NULL,
                        i, j, depth << 3, depth2rgb, depth2rgb + 1);
                depth2rgb += 2;
            }
        }
    }
    texture->UnlockRect(0);
    sensor->NuiImageStreamReleaseFrame(depthStream, &imageFrame);
}

void getRgbHeadData(GLubyte *dest, IFTImage *colorImage) {
    float *fdest = (float *) dest;
    long *depth2rgb = (long *) depthToRgbMap;
    NUI_IMAGE_FRAME imageFrame;
    NUI_LOCKED_RECT LockedRect;


    INuiFrameTexture *texture = imageFrame.pFrameTexture;
    texture->LockRect(0, &LockedRect, NULL, 0);
    if (LockedRect.Pitch != 0) {
        const BYTE *start = (const BYTE *) LockedRect.pBits;
        for (int j = 0; j < height; ++j) {
            for (int i = 0; i < width; ++i) {
                // Determine rgb color for each depth pixel
                long x = *depth2rgb++;
                long y = *depth2rgb++;
                // If out of bounds, then don't color it at all
                if (x < 0 || y < 0 || x > width || y > height) {
                    for (int n = 0; n < 3; ++n) *(fdest++) = 0.0f;
                } else {
                    const BYTE *curr = start + (x + width * y) * 4;
                    for (int n = 0; n < 3; ++n) *(fdest++) = curr[2 - n] / 255.0f;
                }

            }
        }
    }
    texture->UnlockRect(0);
    sensor->NuiImageStreamReleaseFrame(rgbStream, &imageFrame);
}

void getRgbData(GLubyte *dest) {
    float *fdest = (float *) dest;
    long *depth2rgb = (long *) depthToRgbMap;
    NUI_IMAGE_FRAME imageFrame;
    NUI_LOCKED_RECT LockedRect;
    if (sensor->NuiImageStreamGetNextFrame(rgbStream, 0, &imageFrame) < 0) return;
    INuiFrameTexture *texture = imageFrame.pFrameTexture;
    texture->LockRect(0, &LockedRect, NULL, 0);
    if (LockedRect.Pitch != 0) {
        const BYTE *start = (const BYTE *) LockedRect.pBits;
        for (int j = 0; j < height; ++j) {
            for (int i = 0; i < width; ++i) {
                // Determine rgb color for each depth pixel
                long x = *depth2rgb++;
                long y = *depth2rgb++;
                // If out of bounds, then don't color it at all
                if (x < 0 || y < 0 || x > width || y > height) {
                    for (int n = 0; n < 3; ++n) *(fdest++) = 0.0f;
                } else {
                    const BYTE *curr = start + (x + width * y) * 4;
                    for (int n = 0; n < 3; ++n) *(fdest++) = curr[2 - n] / 255.0f;
                }

            }
        }
    }
    texture->UnlockRect(0);
    sensor->NuiImageStreamReleaseFrame(rgbStream, &imageFrame);
}

void getKinectHeadData() {
    GLuint &vboIdPtr = getVboId();
    GLuint &cboIdPtr = getCboId();
    const int dataSize = width * height * 3 * 4;
    //GLubyte *ptr;
    //glBindBuffer(GL_ARRAY_BUFFER, vboIdPtr);
    //ptr = (GLubyte *) glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    //getHeadColorImage(ptr);
    //glUnmapBuffer(GL_ARRAY_BUFFER);
    //getSkeletalData();
}

void getKinectData() {
    GLuint &vboIdPtr = getVboId();
    GLuint &cboIdPtr = getCboId();
    const int dataSize = width * height * 3 * 4;
    GLubyte *ptr;
    glBindBuffer(GL_ARRAY_BUFFER, vboIdPtr);
    ptr = (GLubyte *) glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    if (ptr) {
        getDepthData(ptr);
    }
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glBindBuffer(GL_ARRAY_BUFFER, cboIdPtr);
    ptr = (GLubyte *) glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    if (ptr) {
        getRgbData(ptr);
    }
    glUnmapBuffer(GL_ARRAY_BUFFER);
    getSkeletalData();
}

Json::Value fillJoint(Json::Value json, std::string jointIdentifier, JointData input) {
    json[jointIdentifier]["rotation"]["x"] = input.rotation.x;
    json[jointIdentifier]["rotation"]["y"] = input.rotation.y;
    json[jointIdentifier]["rotation"]["z"] = input.rotation.z;
    json[jointIdentifier]["rotation"]["w"] = input.rotation.w;

    if(writeToCSVFile){
        std::ofstream myfile;
        myfile.open ("../kinect_t_pose.csv", std::ios_base::app);
        myfile << input.rotation.x << "," << input.rotation.y << "," << input.rotation.z << "," << input.rotation.w << ",";
        myfile.close();
    }

    //json[jointIdentifier]["M11"] = input.rotationMatrix.M11;
    //json[jointIdentifier]["M12"] = input.rotationMatrix.M12;
    //json[jointIdentifier]["M13"] = input.rotationMatrix.M13;
    //json[jointIdentifier]["M14"] = input.rotationMatrix.M14;
    //json[jointIdentifier]["M21"] = input.rotationMatrix.M21;
    //json[jointIdentifier]["M22"] = input.rotationMatrix.M22;
    //json[jointIdentifier]["M23"] = input.rotationMatrix.M23;
    //json[jointIdentifier]["M24"] = input.rotationMatrix.M24;
    //json[jointIdentifier]["M31"] = input.rotationMatrix.M31;
    //json[jointIdentifier]["M32"] = input.rotationMatrix.M32;
    //json[jointIdentifier]["M33"] = input.rotationMatrix.M33;
    //json[jointIdentifier]["M34"] = input.rotationMatrix.M34;
    //json[jointIdentifier]["M41"] = input.rotationMatrix.M41;
    //json[jointIdentifier]["M42"] = input.rotationMatrix.M42;
    //json[jointIdentifier]["M43"] = input.rotationMatrix.M43;
    //json[jointIdentifier]["M44"] = input.rotationMatrix.M44;


    json[jointIdentifier]["position"]["x"] = input.position.x;
    json[jointIdentifier]["position"]["y"] = input.position.y;
    json[jointIdentifier]["position"]["z"] = input.position.z;
    return json;
}

void fillKinectIntoJson() {
    getKinectData();

    Json::Value json;

    if(writeToCSVFile){
        std::ofstream myfile;
        myfile.open ("../kinect_t_pose.csv", std::ios_base::app);
        myfile << std::time(0) << ",";
        myfile.close();
    }

    json = fillJoint(json, "hip-center", skeletonStructure[NUI_SKELETON_POSITION_HIP_CENTER]);
    json = fillJoint(json, "spine", skeletonStructure[NUI_SKELETON_POSITION_SPINE]);
    json = fillJoint(json, "shoulder-center", skeletonStructure[NUI_SKELETON_POSITION_SHOULDER_CENTER]);
    json = fillJoint(json, "head", skeletonStructure[NUI_SKELETON_POSITION_HEAD]);
    json = fillJoint(json, "shoulder-left", skeletonStructure[NUI_SKELETON_POSITION_SHOULDER_LEFT]);
    json = fillJoint(json, "elbow-left", skeletonStructure[NUI_SKELETON_POSITION_ELBOW_LEFT]);
    json = fillJoint(json, "wrist-left", skeletonStructure[NUI_SKELETON_POSITION_WRIST_LEFT]);
    json = fillJoint(json, "hand-left", skeletonStructure[NUI_SKELETON_POSITION_HAND_LEFT]);
    json = fillJoint(json, "shoulder-right", skeletonStructure[NUI_SKELETON_POSITION_SHOULDER_RIGHT]);
    json = fillJoint(json, "elbow-right", skeletonStructure[NUI_SKELETON_POSITION_ELBOW_RIGHT]);
    json = fillJoint(json, "wrist-right", skeletonStructure[NUI_SKELETON_POSITION_WRIST_RIGHT]);
    json = fillJoint(json, "hand-right", skeletonStructure[NUI_SKELETON_POSITION_HAND_RIGHT]);
    json = fillJoint(json, "hip-left", skeletonStructure[NUI_SKELETON_POSITION_HIP_LEFT]);
    json = fillJoint(json, "knee-left", skeletonStructure[NUI_SKELETON_POSITION_KNEE_LEFT]);
    json = fillJoint(json, "ankle-left", skeletonStructure[NUI_SKELETON_POSITION_ANKLE_LEFT]);
    json = fillJoint(json, "foot-left", skeletonStructure[NUI_SKELETON_POSITION_FOOT_LEFT]);
    json = fillJoint(json, "hip-right", skeletonStructure[NUI_SKELETON_POSITION_HIP_RIGHT]);
    json = fillJoint(json, "knee-right", skeletonStructure[NUI_SKELETON_POSITION_KNEE_RIGHT]);
    json = fillJoint(json, "ankle-right", skeletonStructure[NUI_SKELETON_POSITION_ANKLE_RIGHT]);
    json = fillJoint(json, "foot-right", skeletonStructure[NUI_SKELETON_POSITION_FOOT_RIGHT]);

    if(writeToCSVFile){
        std::ofstream myfile;
        myfile.open ("../kinect_t_pose.csv", std::ios_base::app);
        myfile << "\n";
        myfile.close();
    }



    std::string styledJsonString = json.toStyledString();
    // Uglyfy Json
    boost::replace_all(styledJsonString,"\n","");
    boost::replace_all(styledJsonString," ","");

    jsonData = styledJsonString;

}

std::string *getJson() {
    return &jsonData;
}


std::array<Vector4, NUI_SKELETON_POSITION_COUNT> *getSkeletonPosition() {
    return &skeletonPosition;
}

std::list<Vector4> *getHeadData() {
    return &headData;
}

BOOL getHeadColorImage(GLubyte *dest) {
    BOOL ret = TRUE;

    // Now, copy a fraction of the camera image into the screen.
    IFTImage *colorImage = m_FTHelper.GetColorImage();

    if (colorImage) {
        int iWidth = colorImage->GetWidth();
        int iHeight = colorImage->GetHeight();

        GLuint &vboIdPtr = getVboId();
        GLuint &cboIdPtr = getCboId();
        const int dataSize = width * height * 3 * 4;
        if (dest) {

            float *fdest = (float *) dest;
            long *depth2rgb = (long *) depthToRgbMap;

            const BYTE *start = (const BYTE *) m_FTHelper.GetColorImage()->GetBytesPerPixel();
            for (int j = 0; j < height; ++j) {
                for (int i = 0; i < width; ++i) {
                    // Determine rgb color for each depth pixel
                    long x = *depth2rgb++;
                    long y = *depth2rgb++;
                    // If out of bounds, then don't color it at all
                    if (x < 0 || y < 0 || x > width || y > height) {
                        for (int n = 0; n < 3; ++n) *(fdest++) = 0.0f;
                    } else {
                        const BYTE *curr = start + (x + width * y) * 4;
                        for (int n = 0; n < 3; ++n) *(fdest++) = curr[2 - n] / 255.0f;
                    }

                }
            }
            //getDepthHeadData(ptr);
        }
    }
    return ret;
}
