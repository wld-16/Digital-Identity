#include "glut.h"
#include "main.h"
#include <iostream>
#include <cstdio>
#include "kinect_websocket.h"
#include "kinect_processor.h"

#include <iostream>

// We'll be using buffer objects to store the kinect point cloud
GLuint vboId;
GLuint cboId;

void draw() {
   drawKinectData();
   glutSwapBuffers();
}

void drawKinectData() {
    getKinectData();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

    glBindBuffer(GL_ARRAY_BUFFER, vboId);
    glVertexPointer(3, GL_FLOAT, 0, NULL);

    glBindBuffer(GL_ARRAY_BUFFER, cboId);
    glColorPointer(3, GL_FLOAT, 0, NULL);

    fillKinectIntoJson();

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
}

void execute() {
    glutMainLoop();
}

bool init(int argc, char* argv[]) {
    argc = 0;
    argv = NULL;
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowSize(width,height);
    glutCreateWindow("Kinect SDK Tutorial");
    glutDisplayFunc(draw);
    glutIdleFunc(draw);
	glewInit();
    return true;
}

void initGL(){
    // OpenGL setup
    glClearColor(0, 0, 0, 0);
    glClearDepth(1.0f);

    // Set up array buffers
    const int dataSize = width * height * 3 * 4;
    glGenBuffers(1, &vboId);
    glBindBuffer(GL_ARRAY_BUFFER, vboId);
    glBufferData(GL_ARRAY_BUFFER, dataSize, 0, GL_DYNAMIC_DRAW);
    glGenBuffers(1, &cboId);
    glBindBuffer(GL_ARRAY_BUFFER, cboId);
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

GLuint &getVboId(){
    return vboId;
};
GLuint &getCboId(){
    return cboId;
};