//
// Created by wnabo on 15.11.2020.
//

#ifndef KINECT_API_KINECT_WEBSOCKET_H
#define KINECT_API_KINECT_WEBSOCKET_H


#define DEFAULT_BUFLEN 1024

#include <functional>
#include <WinSock2.h>
#include <websocketpp/endpoint.hpp>
#include <websocketpp/server.hpp>
#include <websocketpp/config/core.hpp>
#include <websocketpp/config/asio_no_tls.hpp>

using namespace std;

class kinect_websocket {
public:
    typedef websocketpp::server<websocketpp::config::asio> server;
    kinect_websocket() ;
    void run() ;
    void setJson(std::string *json);

};


#endif //KINECT_API_KINECT_WEBSOCKET_H
