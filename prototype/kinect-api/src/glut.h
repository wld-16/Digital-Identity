#pragma once
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <gl/glew.h>
#include <gl/GL.h>
#include <gl/GLU.h>
#include <string>
#include <GLFW/glfw3.h>

BOOL ShowVideo(HDC hdc, int width, int height, int originX, int originY);
bool initHead(int argc, char* argv[]);
void initGL();
void mainLoop(GLFWwindow* window, GLFWwindow* secondWindow);
void initGL2();
void drawSkeletonCallback();
GLFWwindow* initGLFWWindow(GLFWwindow* window, std::string windowTitle, GLFWwindow* window_2 = NULL);
void execute();
std::string *getJson();
GLuint &getVboId();
GLuint &getCboId();
GLuint &getVboId2();
GLuint &getCboId2();