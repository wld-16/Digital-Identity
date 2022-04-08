#include "glut.h"
#include "App.h"
#include <iostream>
#include <cstdio>
#include "kinect_websocket.h"
#include "kinect_processor.h"

#include <iostream>
#include <GLFW/glfw3.h>

// We'll be using buffer objects to store the kinect point cloud
GLuint vboId_1;
GLuint cboId_1;

GLuint vboId_2;
GLuint cboId_2;

bool isDefaultView = true;

int window_1, window_2;


void zoomAt(Vector4 vector4);

bool drawKinectData() {
    getKinectData(vboId_1, cboId_1);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

    glBindBuffer(GL_ARRAY_BUFFER, vboId_1);
    glVertexPointer(3, GL_FLOAT, 0, NULL);

    glBindBuffer(GL_ARRAY_BUFFER, cboId_1);
    glColorPointer(3, GL_FLOAT, 0, NULL);

    glPointSize(1.f);
    glDrawArrays(GL_POINTS, 0, width * height);

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);

    glBegin(GL_LINES);
    glColor3f(1.f, 0.f, 0.f);

    std::array<Vector4, NUI_SKELETON_POSITION_COUNT> *skeletonPosition = getSkeletonPosition();

    for (size_t i = 0; i < NUI_SKELETON_POSITION_COUNT; i++) {
        glVertex3f(skeletonPosition->at(i).x, skeletonPosition->at(i).y, skeletonPosition->at(i).z);
    }
    glEnd();
    return getSkeletonFound();
}

void KeyboardCB(unsigned char key, int x, int y){
    switch(key) {
        case 'q':
            std::exit(0);
    }

}

static void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
}

GLFWwindow* initGLFWWindow(GLFWwindow* window, std::string windowTitle, GLFWwindow* window_2) {
    window = glfwCreateWindow(width, height, windowTitle.c_str(), NULL, window_2);
    if (!window)
    {
        glfwTerminate();
        exit(EXIT_FAILURE);
    }

    if(window_2 != NULL) {
        int width2 = 0;
        int left = 0;
        int right = 0;
        int xpos = 0;
        int ypos = 0;
        glfwGetWindowSize(window_2, &width2, NULL);
        glfwGetWindowFrameSize(window, &left, NULL, &right, NULL);
        glfwGetWindowPos(window_2, &xpos, &ypos);

        glfwSetWindowPos(window, xpos + width + left + right, ypos);

    }

    glfwMakeContextCurrent(window);
    glfwSwapInterval(1);
    glfwSetKeyCallback(window, key_callback);
    glewInit();
    return window;
}

void mainLoop(GLFWwindow* window, GLFWwindow* secondWindow) {
    while (!glfwWindowShouldClose(window) || !glfwWindowShouldClose(secondWindow))
    {
        glfwMakeContextCurrent(window);
        // Kinect Code here
        drawKinectData();

        glfwSwapBuffers(window);
        glfwPollEvents();

        glfwMakeContextCurrent(secondWindow);
        // Kinect Code here
        bool foundSkeleton = drawKinectData();

        if(foundSkeleton) {
            Vector4 focusPosition = getSkeletonPosition()->at(NUI_SKELETON_POSITION_SPINE);
            Vector4 focusEulerAngle = getBonesEulerAngles()->at(NUI_SKELETON_POSITION_SPINE);

            glColor3f(0.f, 1.f, 0.f);
            glBegin(GL_LINES);
                glVertex3f(focusPosition.x, focusPosition.y, focusPosition.z);
                glVertex3f(focusPosition.x + focusEulerAngle.x, focusPosition.y, focusPosition.z);
            glEnd();

            glColor3f(0.f, 0.f, 1.f);
            glBegin(GL_LINES);
                glVertex3f(focusPosition.x, focusPosition.y, focusPosition.z);
                glVertex3f(focusPosition.x, focusPosition.y + focusEulerAngle.y, focusPosition.z);
            glEnd();

            glColor3f(0.f, 1.f, 1.f);
            glBegin(GL_LINES);
                glVertex3f(focusPosition.x, focusPosition.y, focusPosition.z);
                glVertex3f(focusPosition.x, focusPosition.y, focusPosition.z + focusEulerAngle.z);
            glEnd();

            zoomAt(focusPosition);
        }

        glfwSwapBuffers(secondWindow);
        glfwPollEvents();

    }
}

void zoomAt(Vector4 vector4) {
    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(20, width / (GLdouble) height, 0.1, 1000);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0, 0, 0, vector4.x, vector4.y, vector4.z, 0, 1, 0);
}

void rotateCamera() {
    static double angle = 0.;
    static double radius = 3.;
    double x = radius * sin(angle);
    double z = radius * (1 - cos(angle)) - radius / 2;
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(x, 0, z, 0, 0, radius / 2, 0, 1, 0);
    angle += 0.05;
}


void initGL(){
    // OpenGL setup
    glClearColor(0, 0, 0, 0);
    glClearDepth(1.0f);

    // Set up array buffers
    const int dataSize = width * height * 3 * 4;
    glGenBuffers(1, &vboId_1);
    glBindBuffer(GL_ARRAY_BUFFER, vboId_1);
    glBufferData(GL_ARRAY_BUFFER, dataSize, 0, GL_DYNAMIC_DRAW);
    glGenBuffers(1, &cboId_1);
    glBindBuffer(GL_ARRAY_BUFFER, cboId_1);
    glBufferData(GL_ARRAY_BUFFER, dataSize, 0, GL_DYNAMIC_DRAW);

    // Camera setup
    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45, width / (GLdouble) height, 0.1, 1000);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0, 0, 0, 0, 0, 1, 0, 1, 0);
}

void initGL2(){
    // OpenGL setup
    glClearColor(0, 0, 0, 0);
    glClearDepth(1.0f);

    // Set up array buffers
    const int dataSize = width * height * 3 * 4;
    glGenBuffers(1, &vboId_2);
    glBindBuffer(GL_ARRAY_BUFFER, vboId_2);
    glBufferData(GL_ARRAY_BUFFER, dataSize, 0, GL_DYNAMIC_DRAW);
    glGenBuffers(1, &cboId_2);
    glBindBuffer(GL_ARRAY_BUFFER, cboId_2);
    glBufferData(GL_ARRAY_BUFFER, dataSize, 0, GL_DYNAMIC_DRAW);

    // Camera setup
    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45, width / (GLdouble) height, 0.1, 1000);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0, 0, 0, 0, 0, 1, 0, 1, 0);
}

GLuint &getVboId() {
    return vboId_1;
}

GLuint &getCboId() {
    return cboId_1;
}

GLuint &getVboId2() {
    return vboId_2;
}

GLuint &getCboId2() {
    return cboId_1;
}