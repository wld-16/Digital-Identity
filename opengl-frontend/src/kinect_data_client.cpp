//
// Created by wnabo on 20.11.2020.
//

#include "kinect_data_client.h"
#include <websocketpp/config/asio_no_tls_client.hpp>
#include <websocketpp/client.hpp>
#include <GL/glew.h>
#include <GL/glut.h>

#include <iostream>

typedef websocketpp::client<websocketpp::config::asio_client> client;

using websocketpp::lib::placeholders::_1;
using websocketpp::lib::placeholders::_2;
using websocketpp::lib::bind;
using nlohmann::json;

// pull out the type of messages sent by our config
typedef websocketpp::config::asio_client::message_type::ptr message_ptr;

// Create a client endpoint
client::connection_ptr con;
std::unique_ptr<kinect_data_client> instance_pointer;

json jsonFrame;

// This message handler will be invoked once for each incoming message. It
// prints the message and then sends a copy of the message back to the server.
void on_message(client* c, websocketpp::connection_hdl hdl, message_ptr msg) {

    websocketpp::lib::error_code ec;

    jsonFrame = json::parse(msg->get_payload());

    instance_pointer->pushQueue(msg->get_payload());

    c->send(hdl, msg->get_payload(), msg->get_opcode(), ec);

    if (ec) {
        std::cout << "Echo failed because: " << ec.message() << std::endl;
    }
}

void on_connection(client* c, client::connection_ptr con){
    std::cout << "Connected to " << con->get_host() << ":" << con->get_port() << std::endl;
}

void kinect_data_client::readJson(){
    if(con != nullptr){
        con.get()->send("test");
    }
}


int kinect_data_client::run(){
    client c;
    std::string uri = "ws://localhost:9002";


    try {
        // Set logging to be pretty verbose (everything except message payloads)
        c.set_access_channels(websocketpp::log::alevel::connect);
        c.clear_access_channels(websocketpp::log::alevel::none);

        // Initialize ASIO
        c.init_asio();

        // Register our message handler
        c.set_message_handler(bind(&on_message,&c,::_1,::_2));
        websocketpp::lib::error_code ec;

        c.set_open_handler(bind(&on_connection, &c, c.get_connection(uri, ec)));

        con = c.get_connection(uri, ec);

        if (ec) {
            std::cout << "could not create connection because: " << ec.message() << std::endl;
            return 0;
        }

        // Note that connect here only requests a connection. No network messages are
        // exchanged until the event loop starts running in the next line.
        c.connect(con);

        std::cout << "Running Client" << std::endl;
        // Start the ASIO io_service run loop
        // this will cause a single connection to be made to the server. c.run()
        // will exit when this connection is closed.
        c.run();

    } catch (websocketpp::exception const & e) {
        std::cout << e.what() << std::endl;
    }
    return 0;
}

json kinect_data_client::getLastJsonFrame() {
    return jsonFrame;
}

bool kinect_data_client::attemptPopJsonQueue(json &json) {
    if(this->jsonQueue.size() < 10){
        return false;
    } else {
        json = json::parse(this->jsonQueue.front());
        this->jsonQueue.pop();
        return true;
    }
}

void kinect_data_client::pushQueue(
        std::string json) {
    this->jsonQueue.emplace(json);

}

kinect_data_client::kinect_data_client() {
    instance_pointer = std::unique_ptr<kinect_data_client>(this);
}
