//
// Created by wnabo on 09.12.2020.
//

#ifndef DIGITAL_IDENTITY_CALIBRATION_H
#define DIGITAL_IDENTITY_CALIBRATION_H


#include <array>
#include <map>
#include <string>
#include <assimp/quaternion.h>
#include <unordered_map>
#include "ogl/math_3d.h"

class Calibration {
public:
    map<string, aiQuaternion> get_calibration_result();
    std::array<std::pair<std::string, aiQuaternion>, 20> get_kinect_t_pose_orientation();
    void set_output_t_pose_orientation_offset(std::map<std::string, aiQuaternion> quatMap);

    void calculateOrientationInverse();
    void setJointIdentifiers(array<string, 20> array);

private:
    aiQuaternion calculateCalibrationRotation(aiQuaternion kinectTPoseaiQuaternion, aiQuaternion openGlPoseaiQuaternion);
    float quatMagnitude(aiQuaternion quaternion);
    std::map<std::string, aiQuaternion> t_pose_orientations_map;
    std::map<std::string, aiQuaternion> joint_orientation_inverse_map;
    std::array<std::string, 20> jointIdentifiers;

    std::array<std::pair<std::string, aiQuaternion>, 20> initialKinectTPoseOrientations =
            {std::make_pair<>("knee-right", aiQuaternion(0.01447065, 0.99907, -0.00456964, -0.03207825)),
             std::make_pair<>("foot-right", aiQuaternion(-0.3953675, 0.9129865, -0.0739987, -0.03178965)),
             std::make_pair<>("ankle-right", aiQuaternion(-0.04967315, 0.9969415, -0.0310174, -0.0330079)),
             std::make_pair<>("hip-right", aiQuaternion(-0.0988443, 0.918139, 0.3840745, 0.00400616)),
             std::make_pair<>("knee-left", aiQuaternion(0.02700465, 0.9979055, 0.04234005, -0.0328532)),
             std::make_pair<>("ankle-left", aiQuaternion(-0.0269126, 0.9977545, 0.008798065, -0.03568045)),
             std::make_pair<>("foot-left", aiQuaternion(0.5228735, -0.763843, -0.144959, -0.0686177)),
             std::make_pair<>("hip-left", aiQuaternion(-0.0712803, 0.9240805, -0.3693425, -0.06676265)),
             std::make_pair<>("hip-center", aiQuaternion(-0.03351965, -0.009073065, 0.995034, 0.09072275)),
             std::make_pair<>("spine", aiQuaternion(-0.0162731, 0.002123155, 0.9287645, 0.3632395)),
             std::make_pair<>("shoulder-left", aiQuaternion(0.02226875, 0.8485265, -0.522291, -0.02776955)),
             std::make_pair<>("shoulder-center", aiQuaternion(-0.01697975, -0.005901285, 0.999529, 0.00961009)),
             std::make_pair<>("head", aiQuaternion(-0.02281365, -0.0497664, 0.9904365, -0.106651)),
             std::make_pair<>("elbow-left", aiQuaternion(0.05812835, 0.7262695, -0.670384, -0.0641654)),
             std::make_pair<>("wrist-left", aiQuaternion(0.0351973, 0.727102, -0.663893, -0.04344975)),
             std::make_pair<>("shoulder-right", aiQuaternion(0.00359886, 0.8487455, 0.5275435, 0.00359769)),
             std::make_pair<>("elbow-right", aiQuaternion(0.3784455, 0.672105, 0.5041685, -0.3186035)),
             std::make_pair<>("wrist-right", aiQuaternion(0.306324, 0.6215565, 0.5550885, -0.369236)),
             std::make_pair<>("hand-right", aiQuaternion(0.5886755, 0.594687, -0.416206, 0.186519)),
             std::make_pair<>("hand-left", aiQuaternion(-0.03300835, 0.7168985, -0.545081, 0.03694175))};

    aiQuaternion scalarMultiplication(aiQuaternion quaternion, float scalar);
};


#endif //DIGITAL_IDENTITY_CALIBRATION_H
