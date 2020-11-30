//
// Created by wnabo on 29.11.2020.
//

#ifndef DIGITAL_IDENTITY_JSON_FILE_READER_H
#define DIGITAL_IDENTITY_JSON_FILE_READER_H


#include <string>
#include "json/json.hpp"

class json_file_reader {

public:
    //virtual ~json_file_reader();

public:
    void run(std::string readPath);
    //std::shared_ptr<nlohmann::json> getJsonData();

private:
    void read(std::string readPath);
    void loadData(std::string data);
};


#endif //DIGITAL_IDENTITY_JSON_FILE_READER_H
