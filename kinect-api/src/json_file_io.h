//
// Created by wnabo on 29.11.2020.
//

#ifndef KINECT_API_JSON_FILE_IO_H
#define KINECT_API_JSON_FILE_IO_H


#include <string>

class json_file_io {
public:
    void setJson(std::string *jsonString);
    void setWritePath(std::string writePath);
    void run();
private:
    void write();
    std::string writePath;
};


#endif //KINECT_API_JSON_FILE_IO_H
