# Digital-identity

### Project 

This Repository contains all the data related the Digital Identity and the related scientific studies.

### Prototype

The prototype directory contains two subdirectories: kinect-api and OpenGL Frontend.
Under the 'kinect-api' directory the Kinect SDK is used to serve data via Websockets to a client. 
Under the other directory, 'opengl-frontend' the client side is implemented. Here the clientside attempts to renders the data onto an via OpenGL rendered skeleton, it still has some todos.

Tested with:
- Kinect SDK 1.8

### Demo

The unity-skelton-visulize contains an Unity Project that was presented at the [Digital Identity](https://www.digital-identity.live/). The main scene was at presentation. Under the scenes folder the study folder contains scenes to print data from the arduino and the kinect. The Kalman filter was also attempted to be implemented, attempts to implement Push-Pull interface for specific components and a Kalman filter that can be configured via the Unity Inspector, maybe heading into the direction of publishing a plugin for Unity. 

Tested with:
- Unity 2019.4.8f1
- Kinect SDK 1.8

Also make sure to install the Kinect with MS-SDK Package published by RF Solutions from the Asset store.

### Studies

Studies have been conducted with an Inertial Measurement Unit (IMU). For this a small operating system has been programmed, to select the data to be output and in which format. For our studies we output in JSON, then the data will be read by Unity in that format. Here a scene exists, were both sensors output in order to merged the data into one .csv file.
This csv file will then be further examined in the RStudio Project, that can be found under the data_science directory.
Simulations of the implenting orientation and acceleration model have been written and tested. Then the real data was processed in another worksheet. Plots can be output, when the the worksheet is written and parameters are set. 


Data Science
Tested with 
- RStudio Version 2021.09.1 Build 372
- R-4.1.2
