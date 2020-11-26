#include <websocketpp/config/core.hpp>

#include <websocketpp/config/asio_no_tls.hpp>
#include <websocketpp/server.hpp>

#include <iostream>
#include "kinect_websocket.h"
#pragma comment(lib,"Ws2_32.lib")


typedef websocketpp::server<websocketpp::config::asio> server;
void on_message(websocketpp::connection_hdl conn, server::message_ptr msg);
server print_server;
std::string *data;

kinect_websocket::kinect_websocket() {

    print_server.set_message_handler(&on_message);
    print_server.set_access_channels(websocketpp::log::alevel::all);
    print_server.set_error_channels(websocketpp::log::elevel::all);

    print_server.init_asio();
    print_server.listen(9002);
    print_server.start_accept();

    std::cout << "Running Server on 9002" << std::endl;
}


void kinect_websocket::run() {
    print_server.run();
}

void kinect_websocket::setJson(std::string *json) {
    data = json;
}

void on_message(websocketpp::connection_hdl conn, server::message_ptr msg) {
    std::cout << msg->get_payload() << std::endl;

    print_server.send(conn,*data,websocketpp::frame::opcode::TEXT);
}