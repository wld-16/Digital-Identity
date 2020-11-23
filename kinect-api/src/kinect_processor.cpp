//
// Created by wnabo on 18.11.2020.
//


#include <string>
#include "kinect_processor.h"
#include "main.h"
#include "glut.h"
#include "faceTracking/FTHelper.h"
#include <FaceTrackLib.h>

#include <json/json.h>
#include <GL/glew.h>
#include <array>
#include <list>
#include <iostream>


INuiSensor *sensor;
FTHelper m_FTHelper;
std::string jsonData;
IFTImage* m_pVideoBuffer;

// OpenGL Variables
long depthToRgbMap[width * height * 2];

int frame = 0;

// Stores the coordinates of each joint
std::array<Vector4, NUI_SKELETON_POSITION_COUNT> skeletonPosition;
std::list<Vector4> headData;

// Kinect variables
HANDLE depthStream;
HANDLE rgbStream;


void FTHelperCallingBack(PVOID pVoid)
{
    std::cout << "Callback" << std::endl;
    /*
    SingleFace* pApp = reinterpret_cast<SingleFace*>(pVoid);
    if (pApp)
    {
        IFTResult* pResult = pApp->m_FTHelper.GetResult();
        if (pResult && SUCCEEDED(pResult->GetStatus()))
        {
            FLOAT* pAU = NULL;
            UINT numAU;
            pResult->GetAUCoefficients(&pAU, &numAU);
            FLOAT scale;
            FLOAT rotationXYZ[3];
            FLOAT translationXYZ[3];
            pResult->Get3DPose(&scale, rotationXYZ, translationXYZ);
        }

    }
     */
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

bool initKinectFaceTracking(){
    IFTFaceTracker* pFT = FTCreateFaceTracker();
    if(!pFT)
    {
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
    if( FAILED(hr) )
    {
        // Handle errors
    }
    IFTResult* pFTResult = NULL;
    hr = pFT->CreateFTResult(&pFTResult);
    if(FAILED(hr))
    {
        // Handle errors
    }
    // Prepare image interfaces that hold RGB and depth data
    IFTImage* pColorFrame = FTCreateImage();
    IFTImage* pDepthFrame = FTCreateImage();
    if(!pColorFrame || !pDepthFrame)
    {
        // Handle errors
    }

    //
    m_FTHelper.Init(FTHelperCallingBack,nullptr,NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX,
                    NUI_IMAGE_RESOLUTION_320x240,
                    TRUE,
                    TRUE, // if near mode doesn't work, fall back to default mode
                    NUI_IMAGE_TYPE_COLOR,
                    NUI_IMAGE_RESOLUTION_640x480,
                    FALSE);
    return true;
}

void getSkeletalData() {
    NUI_SKELETON_FRAME skeletonFrame = {0};
    if (sensor->NuiSkeletonGetNextFrame(0, &skeletonFrame) >= 0) {
        sensor->NuiTransformSmooth(&skeletonFrame, NULL);
        // Loop over all sensed skeletons
        for (int z = 0; z < NUI_SKELETON_COUNT; ++z) {
            const NUI_SKELETON_DATA &skeleton = skeletonFrame.SkeletonData[z];
            // Check the state of the skeleton
            if (skeleton.eTrackingState == NUI_SKELETON_TRACKED) {
                // Copy the joint positions into our array
                for (int i = 0; i < NUI_SKELETON_POSITION_COUNT; ++i) {
                    skeletonPosition[i] = skeleton.SkeletonPositions[i];
                    if (skeleton.eSkeletonPositionTrackingState[i] == NUI_SKELETON_POSITION_NOT_TRACKED) {
                        skeletonPosition[i].w = 0;
                    }
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

void getRgbHeadData(GLubyte *dest, IFTImage* colorImage){
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
    //getSkeletalData();
}

Json::Value fillJoint(Json::Value json, std::string jointIdentifier, Vector4 input) {
    json[jointIdentifier]["x"] = input.x;
    json[jointIdentifier]["y"] = input.y;
    json[jointIdentifier]["z"] = input.z;
    json[jointIdentifier]["w"] = input.w;
    return json;
}

void fillKinectIntoJson() {
    getKinectData();

    Json::Value json;

    json = fillJoint(json, "hip-center", skeletonPosition[NUI_SKELETON_POSITION_HIP_CENTER]);
    json = fillJoint(json, "spine", skeletonPosition[NUI_SKELETON_POSITION_SPINE]);
    json = fillJoint(json, "shoulder-center", skeletonPosition[NUI_SKELETON_POSITION_SHOULDER_CENTER]);
    json = fillJoint(json, "head", skeletonPosition[NUI_SKELETON_POSITION_HEAD]);
    json = fillJoint(json, "shoulder-left", skeletonPosition[NUI_SKELETON_POSITION_SHOULDER_LEFT]);
    json = fillJoint(json, "elbow-left", skeletonPosition[NUI_SKELETON_POSITION_ELBOW_LEFT]);
    json = fillJoint(json, "wrist-left", skeletonPosition[NUI_SKELETON_POSITION_WRIST_LEFT]);
    json = fillJoint(json, "hand-left", skeletonPosition[NUI_SKELETON_POSITION_HAND_LEFT]);
    json = fillJoint(json, "shoulder-right", skeletonPosition[NUI_SKELETON_POSITION_SHOULDER_RIGHT]);
    json = fillJoint(json, "elbow-right", skeletonPosition[NUI_SKELETON_POSITION_ELBOW_RIGHT]);
    json = fillJoint(json, "wrist-right", skeletonPosition[NUI_SKELETON_POSITION_WRIST_RIGHT]);
    json = fillJoint(json, "hand-right", skeletonPosition[NUI_SKELETON_POSITION_HAND_RIGHT]);
    json = fillJoint(json, "hip-left", skeletonPosition[NUI_SKELETON_POSITION_HIP_LEFT]);
    json = fillJoint(json, "knee-left", skeletonPosition[NUI_SKELETON_POSITION_KNEE_LEFT]);
    json = fillJoint(json, "ankle-left", skeletonPosition[NUI_SKELETON_POSITION_ANKLE_LEFT]);
    json = fillJoint(json, "foot-left", skeletonPosition[NUI_SKELETON_POSITION_FOOT_LEFT]);
    json = fillJoint(json, "hip-right", skeletonPosition[NUI_SKELETON_POSITION_HIP_RIGHT]);
    json = fillJoint(json, "knee-right", skeletonPosition[NUI_SKELETON_POSITION_KNEE_RIGHT]);
    json = fillJoint(json, "ankle-right", skeletonPosition[NUI_SKELETON_POSITION_ANKLE_RIGHT]);
    json = fillJoint(json, "foot-right", skeletonPosition[NUI_SKELETON_POSITION_FOOT_RIGHT]);

    jsonData = json.toStyledString();

}

std::string *getJson(){
    return &jsonData;
}

std::array<Vector4, NUI_SKELETON_POSITION_COUNT> *getSkeletonPosition(){
    return &skeletonPosition;
}

std::list<Vector4> *getHeadData(){
    return &headData;
}

BOOL getImage(GLubyte *dest, int originX, int originY)
{
    BOOL ret = TRUE;

    // Now, copy a fraction of the camera image into the screen.
    IFTImage* colorImage = m_FTHelper.GetColorImage();
    if (colorImage)
    {
        int iWidth = colorImage->GetWidth();
        int iHeight = colorImage->GetHeight();
        if (iWidth > 0 && iHeight > 0)
        {
            int iTop = 0;
            int iBottom = iHeight;
            int iLeft = 0;
            int iRight = iWidth;

            // Keep a separate buffer.
            if (m_pVideoBuffer && SUCCEEDED(m_pVideoBuffer->Allocate(iWidth, iHeight, FTIMAGEFORMAT_UINT8_B8G8R8A8)))
            {
                // Copy do the video buffer while converting bytes
                colorImage->CopyTo(m_pVideoBuffer, NULL, 0, 0);

                // Compute the best approximate copy ratio.
                float w1 = (float)iHeight * (float)width;
                float w2 = (float)iWidth * (float)height;
                if (w2 > w1 && height > 0)
                {
                    // video image too wide
                    float wx = w1/height;
                    iLeft = (int)max(0, m_FTHelper.GetXCenterFace() - wx / 2);
                    iRight = iLeft + (int)wx;
                    if (iRight > iWidth)
                    {
                        iRight = iWidth;
                        iLeft = iRight - (int)wx;
                    }
                }
                else if (w1 > w2 && width > 0)
                {
                    // video image too narrow
                    float hy = w2/width;
                    iTop = (int)max(0, m_FTHelper.GetYCenterFace() - hy / 2);
                    iBottom = iTop + (int)hy;
                    if (iBottom > iHeight)
                    {
                        iBottom = iHeight;
                        iTop = iBottom - (int)hy;
                    }
                }

                int const bmpPixSize = m_pVideoBuffer->GetBytesPerPixel();


            }
        }
        GLuint &vboIdPtr = getVboId();
        GLuint &cboIdPtr = getCboId();
        const int dataSize = width * height * 3 * 4;
        GLubyte *ptr;
        glBindBuffer(GL_ARRAY_BUFFER, vboIdPtr);
        ptr = (GLubyte *) glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
        if (ptr) {
            //getDepthHeadData(ptr);
        }
        glUnmapBuffer(GL_ARRAY_BUFFER);
        glBindBuffer(GL_ARRAY_BUFFER, reinterpret_cast<GLuint>(colorImage->GetBuffer()));
        ptr = (GLubyte *) glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
        if (ptr) {
            getRgbHeadData(ptr, colorImage);
        }
        glUnmapBuffer(GL_ARRAY_BUFFER);

        colorImage->GetBuffer();
    }
    return ret;
}
