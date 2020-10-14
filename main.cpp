#include <iostream>
#include <stdio.h>
#include <GL/glut.h>


int main(int argc, char** argv) {

    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutCreateWindow("red 3D lighted cube");
    printf("GL_VERSION = %s\n",glGetString(GL_VERSION) ); /* GL_VERSION = 2.1.2 NVIDIA 195.36.24 */
    return 0;
}