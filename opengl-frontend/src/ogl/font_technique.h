//
// Created by wnabo on 03.12.2020.
//

#ifndef DIGITAL_IDENTITY_FONT_TECHNIQUE_H
#define DIGITAL_IDENTITY_FONT_TECHNIQUE_H


#include <technique.h>
#include "math_3d.h"

class FontTechnique : public Technique{
public:
    virtual bool Init();
    void RenderText(std::string text, float x, float y, float scale, Vector3f color);
    int InitFonts();
    unsigned int VAO, VBO;
};


#endif //DIGITAL_IDENTITY_FONT_TECHNIQUE_H
