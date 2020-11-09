//
// Created by wnabo on 02.11.2020.
//

#include "opencv_texture.h"

#include <iostream>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/core/opengl.hpp>

Texture::Texture(GLenum TextureTarget, const std::string& FileName)
{
    m_textureTarget = TextureTarget;
    m_fileName      = FileName;
}


bool Texture::Load()
{
    m_image = cv::imread(m_fileName);
    cv::cvtColor(m_image,m_image,cv::COLOR_BGR2RGB);
    cv::flip(m_image, m_blob,-1);

    if(m_image.empty()){
        std::cout << "image empty" << std::endl;
    }else{
        glGenTextures( 1, &m_textureObj );
        glBindTexture( m_textureTarget, m_textureObj );
        glTexImage2D(GL_TEXTURE_2D,0,GL_RGB,m_blob.cols, m_blob.rows,0,GL_RGB,GL_UNSIGNED_BYTE, m_blob.data);
        glTexParameterf(m_textureTarget,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
        glTexParameterf(m_textureTarget,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
        glTexParameterf( m_textureTarget, GL_TEXTURE_WRAP_S , GL_REPEAT );
        glTexParameterf( m_textureTarget, GL_TEXTURE_WRAP_T, GL_REPEAT );
        return true;
    }
    return false;
}

void Texture::Bind(GLenum TextureUnit)
{
    glActiveTexture(TextureUnit);
    glBindTexture(m_textureTarget, m_textureObj);
}
