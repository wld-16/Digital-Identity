using System.Collections;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Single;
using UnityEditor;
using static System.Math;
using Matrix4x4 = System.Numerics.Matrix4x4;
using UnityEngine;

public class OrientationKalmanFilterBehaviour : GeneralKalmanFilterBehaviour
{
    private void Update()
    {
        
        List<float> data = getDataDeliverer(typeof(FetchOrientationOfKinect)).getData();
        List<float> angleVelocity = getDataDeliverer(typeof(FetchOrientation)).getData();
        
        
        float w = 0;
        float x = 0;
        float y = 0;
        float z = 0;
        float vx = angleVelocity[0];
        float vy = angleVelocity[1];
        float vz = angleVelocity[2];


        Matrix<float> Z = DenseMatrix.OfArray(new float[7, 1]
        {
            {w},
            {x},
            {y},
            {z},
            {vx},
            {vy},
            {vz}
        });
        
        
        kalmanFilter.H = UpdateDynamicFWithQuaternion(new Quaternion(data[1], data[2], data[3], data[0]));


        kalmanFilter = KalmanUpdate(Z, kalmanFilter);


        Dictionary<string, Matrix<float>> kalmanPredict = KalmanPredict(kalmanFilter);

        kalmanResultX = kalmanPredict["x"].ToRowMajorArray().ToList();

        outputData = observationOutputMapping.Select(index => kalmanResultX[index]).ToList();
    }

    Matrix<float> UpdateDynamicFWithQuaternion(Quaternion quat)
    {
        return DenseMatrix.OfArray(new float[7, 7]
        {
            {1f,0f, 0f, 0f, -quat.x * dt / 2, -quat.y, -quat.z * dt / 2},
            {0, 1f, 0, 0, quat.w * dt / 2, quat.z, quat.y * dt / 2},
            {0, 0, 1, 0, quat.z * dt / 2, quat.w * dt / 2, -quat.x * dt / 2},
            {0, 0, 0, 1, -quat.y * dt / 2, quat.x * dt / 2, quat.w * dt / 2},
            {0, 0, 0, 0, 1, 0, 0},
            {0, 0, 0, 0, 0, 1, 0},
            {0, 0, 0, 0, 0, 0, 1}
        });
    }
}