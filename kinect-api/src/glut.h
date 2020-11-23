#pragma once
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <gl/glew.h>
#include <gl/GL.h>
#include <gl/GLU.h>
#include <gl/glut.h>
#include <string>

BOOL ShowVideo(HDC hdc, int width, int height, int originX, int originY);
bool init(int argc, char* argv[]);
bool initHead(int argc, char* argv[]);
void initGL();
void drawSkeletonCallback();
void execute();
std::string *getJson();
GLuint &getVboId();
GLuint &getCboId();