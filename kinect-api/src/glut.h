#pragma once
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <gl/glew.h>
#include <gl/GL.h>
#include <gl/GLU.h>
#include <gl/glut.h>
#include <string>


bool init(int argc, char* argv[]);
bool initKinect();
void initGL();
void draw();
void execute();
std::string *getJson();
GLuint &getVboId();
GLuint &getCboId();