#include <iostream>
#include <stdio.h>
#include <gl/glut.h>

void display (void) {
    //glClearColor (1.0, 1.0, 1.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
   // gluLookAt(0.0,0.5,0,0,0,0,0,1,0);
    glFlush();
}

void Keyboard (unsigned char key, int x, int y) {
    if(key == 27) {
        exit(0);
    }   else printf("Sie haben %c gedrueckt, \n", key);
}

void error_callback(int error, const char* description)
{
    fprintf(stderr, "Error: %s\n", description);
}


int main (int argc, char **argv) {
    glutInit(&argc, argv);
    glutInitWindowSize(500, 500);
    glutInitWindowPosition(100,100);
    glutCreateWindow("Beispiel: Fenster");
    glutDisplayFunc(display);
    glutKeyboardFunc(Keyboard);
    printf("GL_VERSION = %s\n",glGetString(GL_VERSION) );

    glutMainLoop();

    return 0;
}
