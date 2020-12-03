//
// Created by wnabo on 03.12.2020.
//

#include "font_technique.h"

bool FontTechnique::Init() {

    if (!Technique::Init()) {
        return false;
    }

    if (!AddShader(GL_VERTEX_SHADER, "../res/text.vs")) {
        return false;
    }

    if (!AddShader(GL_FRAGMENT_SHADER, "../res/text.fs")) {
        return false;
    }

    if (!Finalize()) {
        return false;
    }
}
