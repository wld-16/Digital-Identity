//
// Created by wnabo on 29.11.2020.
//

#include <fstream>
#include <iostream>
#include "json_file_reader.h"
#include <boost/filesystem.hpp>

void json_file_reader::read(std::string readPath) {
    std::string readString = "";
    std::string line;
    std::ifstream readFile(readPath);
    std::cout << "Will now read" << std::endl;
    if(!boost::filesystem::exists(readPath + ".lock") ){
        std::cout << "Will now write lock" << std::endl;
        std::ofstream output(readPath + ".lock");
        output.close();
        while(getline(readFile, line)){
            readString += line;
        }
        readFile.close();
        boost::filesystem::remove((readPath + ".lock"));
        std::string cloneString(readString);
        loadData(cloneString);

    } else {
        std::cout << "file locked" << std::endl;
    }
}

void json_file_reader::run(std::string readPath) {
    while(true){
        read(readPath);
    }
}

void json_file_reader::loadData(std::string stringData){
    std::cout << "Will now load" << std::endl;
    if(!stringData.empty()){
        std::cout << "data: " + stringData << std::endl;
        nlohmann::json jsonData = nlohmann::json::parse(stringData);
    }
}

/*
std::shared_ptr<nlohmann::json> json_file_reader::getJsonData() {
    return std::make_shared<nlohmann::json>(jsonData);
}

json_file_reader::~json_file_reader() {
    jsonData = NULL;
}
 */
