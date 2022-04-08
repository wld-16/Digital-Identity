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

    void init();
    void performCalibration(map<string, aiQuaternion> skeletonFrame);
    void setJointIdentifiers(array<string, 20> array);

private:
    aiQuaternion calculateCalibrationRotation(aiQuaternion kinectTPoseaiQuaternion, aiQuaternion openGlPoseaiQuaternion,
                                              Vector3f kinectPosition, Vector3f openGlPosition);
    float quatMagnitude(aiQuaternion quaternion);
    std::map<std::string, aiQuaternion> avatar_t_pose_orientations_map;
    std::map<std::string, aiQuaternion> joint_orientation_inverse_map;
    std::array<std::string, 20> jointIdentifiers;

    std::array<std::pair<std::string, aiQuaternion>, 20> initialKinectTPoseOrientations =
            {std::make_pair<>("knee-right", aiQuaternion(-0.0229641, 0.993456, 0.0328639, -0.106948)),
             std::make_pair<>("foot-right", aiQuaternion(-0.40331, 0.912051, 0.0302006, -0.0677563)),
             std::make_pair<>("ankle-right", aiQuaternion(-0.037433, 0.993338, -0.00393497, -0.108913)),
             std::make_pair<>("hip-right", aiQuaternion(-0.0988443, 0.918139, 0.3840745, 0.00400616)),
             std::make_pair<>("knee-left", aiQuaternion(-0.0618177, 0.992277, 0.0214523, -0.105379)),
             std::make_pair<>("ankle-left", aiQuaternion(-0.0657286, 0.991804, 0.032258, -0.104709)),
             std::make_pair<>("foot-left", aiQuaternion(-0.328534, 0.93818, 0.0629229, -0.0890194)),
             std::make_pair<>("hip-left", aiQuaternion(-0.0586738, 0.912075, -0.381542, -0.138209)),
             std::make_pair<>("hip-center", aiQuaternion(-0.106705, 0.0171293,0.988515, 0.105633)),
             std::make_pair<>("spine", aiQuaternion(0.00388853, 0.0455556, 0.916202, 0.398099)),
             std::make_pair<>("shoulder-left", aiQuaternion(0.0939015, 0.742254, -0.65151, -0.125605)),
             std::make_pair<>("shoulder-center", aiQuaternion(-0.0382377, 0.0173248, 0.998753, 0.0270311)),
             std::make_pair<>("head", aiQuaternion(-0.0410843, -0.00342875, 0.99632, -0.0751487)),
             std::make_pair<>("elbow-left", aiQuaternion(0.513175, -0.515734, 0.258428, 0.635519)),
             std::make_pair<>("wrist-left", aiQuaternion(0.621686, -0.377938, 0.403008, 0.555207)),
             std::make_pair<>("shoulder-right", aiQuaternion(0.0477562, 0.82645, 0.559049, 0.0465267)),
             std::make_pair<>("elbow-right", aiQuaternion(0.0226049, 0.730825, 0.679579, 0.0596378)),
             std::make_pair<>("wrist-right", aiQuaternion(-0.0166806, 0.714386, 0.699176, 0.022949)),
             std::make_pair<>("hand-right", aiQuaternion(-0.137704, 0.924193, 0.351174, -0.0598493)),
             std::make_pair<>("hand-left", aiQuaternion(0.566036, -0.496125, 0.292639, 0.589767))};

    aiQuaternion scalarMultiplication(aiQuaternion quaternion, float scalar);

};


#endif //DIGITAL_IDENTITY_CALIBRATION_H
