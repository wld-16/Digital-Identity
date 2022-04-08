//
// Created by wnabo on 09.12.2020.
//

#include <algorithm>
#include "Calibration.h"

aiQuaternion conjugate(aiQuaternion quat){
    quat.x = -quat.x;
    quat.y = -quat.y;
    quat.z = -quat.z;
    return quat;
}

aiQuaternion
Calibration::calculateCalibrationRotation(aiQuaternion kinectTPoseaiQuaternion, aiQuaternion openGlPoseaiQuaternion,
                                          Vector3f kinectPosition, Vector3f openGlPosition) {
    return openGlPoseaiQuaternion *
           scalarMultiplication(conjugate(kinectTPoseaiQuaternion), 1 / quatMagnitude(kinectTPoseaiQuaternion));
}

std::array<std::pair<std::string, aiQuaternion>, 20> Calibration::get_kinect_t_pose_orientation() {
    return initialKinectTPoseOrientations;
}

std::map<std::string, aiQuaternion> Calibration::get_calibration_result() {
    return joint_orientation_inverse_map;
}

float Calibration::quatMagnitude(aiQuaternion aiQuaternion) {
    return sqrt(pow(aiQuaternion.x, 2) + pow(aiQuaternion.y, 2) + pow(aiQuaternion.z, 2) + pow(aiQuaternion.w, 2));
}

void Calibration::init() {
    std::for_each(initialKinectTPoseOrientations.begin(), initialKinectTPoseOrientations.end(),
                  [&](std::pair<std::string, aiQuaternion> jointOrientation) {
                      joint_orientation_inverse_map[jointOrientation.first] = calculateCalibrationRotation(
                              jointOrientation.second, avatar_t_pose_orientations_map[jointOrientation.first],
                              Vector3f(0.0, 0.0, 0.0), Vector3f(0.f,0.f,0.f));
                  });
}

void Calibration::performCalibration(std::map<std::string, aiQuaternion> skeletonFrame) {
    std::for_each(skeletonFrame.begin(), skeletonFrame.end(),
                  [&](std::pair<std::string, aiQuaternion> jointOrientation) {
                      joint_orientation_inverse_map[jointOrientation.first] = calculateCalibrationRotation(
                              jointOrientation.second, avatar_t_pose_orientations_map[jointOrientation.first],
                              Vector3f(0.0, 0.0, 0.0), Vector3f(0.f,0.f,0.f));
                  });
}


void Calibration::set_output_t_pose_orientation_offset(std::map<std::string, aiQuaternion> quatMap) {
    this->avatar_t_pose_orientations_map = quatMap;
}

aiQuaternion Calibration::scalarMultiplication(aiQuaternion quaternion, float scalar) {
    aiQuaternion quat;
    quat.x = quaternion.x * scalar;
    quat.y = quaternion.y * scalar;
    quat.z = quaternion.z * scalar;
    quat.w = quaternion.w * scalar;
    return quat;

}

void Calibration::setJointIdentifiers(array<string, 20> jointIdentifiers) {
    std::for_each(jointIdentifiers.begin(), jointIdentifiers.end(), [&](std::string joint_identifier) {
        joint_orientation_inverse_map[joint_identifier] = aiQuaternion();
    });
    this->jointIdentifiers = jointIdentifiers;
}

// TODO: Quaternion average
