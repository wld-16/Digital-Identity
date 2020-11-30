//
// Created by wnabo on 20.11.2020.
//

#ifndef DIGITAL_IDENTITY_KINECT_DATA_CLIENT_H
#define DIGITAL_IDENTITY_KINECT_DATA_CLIENT_H

#include <queue>
#include "json/json.hpp"

using nlohmann::json;

class kinect_data_client {
public:
    kinect_data_client();

    int run();
    json getLastJsonFrame();
    bool attemptPopJsonQueue(json &json);
    void readJson();

    void init();

    void pushQueue(std::string json);

private:
    std::queue<std::string> jsonQueue;
};


#endif //DIGITAL_IDENTITY_KINECT_DATA_CLIENT_H
