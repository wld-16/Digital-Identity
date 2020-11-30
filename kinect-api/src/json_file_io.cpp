//
// Created by wnabo on 29.11.2020.
//

#include "json_file_io.h"
#include <iostream>
#include <algorithm>
#include <fstream>
#include <boost/filesystem.hpp>
#include <utility>

std::string *fileWritedata;

void json_file_io::setJson(std::string *json) {
    fileWritedata = json;
}

void json_file_io::write(){
    std::ofstream writeFile;
    writeFile.open (writePath,std::ofstream::out | std::ofstream::app);

    std::cout << std::count(fileWritedata->begin(),fileWritedata->end(),'0')<< std::endl;
    if(writeFile.is_open() && !boost::filesystem::exists(writePath + ".lock")){
        std::ofstream output(writePath + ".lock");
        output.close();
        writeFile << *fileWritedata << std::endl;
        writeFile.close();
        boost::filesystem::remove((writePath + ".lock"));
    } else {
        std::cout << "file locked" << std::endl;
    }
}

void json_file_io::run() {
    std::remove("../../test.json");
    std::atexit([]{std::remove("../../test.json.lock");});
    while(true){
        write();
    }
}

void json_file_io::setWritePath(std::string writePath) {
    this->writePath = std::move(writePath);
}
