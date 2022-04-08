//
// Created by wnabo on 02.11.2020.
//

#ifndef DIGITAL_IDENTITY_OPENCV_TEXTURE_H
#define DIGITAL_IDENTITY_OPENCV_TEXTURE_H

#include <string>

#include <opencv2/core.hpp>
#include <opencv2/core/opengl.hpp>
#include <GL/glew.h>


class Texture
{
public:
    Texture(GLenum TextureTarget, const std::string& FileName);

    bool Load();
    void Bind(GLenum TextureUnit);

private:
    std::string m_fileName;
    GLenum m_textureTarget;
    GLuint m_textureObj;
    cv::Mat m_image;
    cv::Mat m_blob;
};


#endif	/* TEXTURE_H */

