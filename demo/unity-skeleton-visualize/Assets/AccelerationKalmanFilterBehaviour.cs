using System.Collections;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Single;
using UnityEditor;
using static System.Math;
using Matrix4x4 = System.Numerics.Matrix4x4;
using UnityEngine;

public class AccelerationKalmanFilterBehaviour : GeneralKalmanFilterBehaviour
{
    private void Update()
    {
        float x = 0;
        float y = 0;
        float z = 0;
        float vx = 0;
        float vy = 0;
        float vz = 0;
        float ax = 0;
        float ay = 0;
        float az = 0;


        Matrix<float> Z = DenseMatrix.OfArray(new float[9, 1]
        {
            {x},
            {y},
            {z},
            {vx},
            {vy},
            {vz},
            {ax},
            {ay},
            {az}
        });



        List<float> data = getDataDeliverer(typeof(OrientationKalmanFilterBehaviour)).getData();
        
        Quaternion quat = new Quaternion(data[0], data[1], data[2], data[3]);

        Matrix<float> dataMatrix = rotationMatrixFromQuaternion(quat.w, quat.x, quat.y, quat.z);
        kalmanFilter.H = UpdateDynamicFWithTransformation(dataMatrix);

        kalmanFilter = KalmanUpdate(Z, kalmanFilter);

        Dictionary<string, Matrix<float>> kalmanPredict = KalmanPredict(kalmanFilter);

        kalmanResultX = kalmanPredict["x"].ToRowMajorArray().ToList();

        outputData = observationOutputMapping.Select(index => kalmanResultX[index]).ToList();
    }
    
    Matrix<float> UpdateDynamicFWithTransformation(Matrix<float> handToLocal)
    {
        return DenseMatrix.OfArray(new float[9, 9]
        {
            {
                1f, dt, (float) (handToLocal[0, 0] * Pow(dt, 2) / 2), 0, 0,
                (float) (handToLocal[1, 0] * Pow(dt, 2) / 2), 0, 0, (float) (handToLocal[2, 0] * Pow(dt, 2) / 2)
            },
            {0, 1, handToLocal[0, 0] * dt, 0, 0, handToLocal[1, 0] * dt, 0, 0, handToLocal[2, 0] * dt},
            {0, 0, 1, 0, 0, 0, 0, 0, 0},
            {
                0, dt, (float) (handToLocal[0, 1] * Pow(dt, 2) / 2), 1, dt,
                (float) (handToLocal[1, 1] * Pow(dt, 2) / 2), 0, 0, (float) (handToLocal[2, 2] * Pow(dt, 2) / 2)
            },
            {0, 0, handToLocal[0, 1] * dt, 0, 1, handToLocal[1, 1] * dt, 0, 0, handToLocal[2, 1] * dt},
            {0, 0, 0, 0, 0, 1, 0, 0, 0},
            {
                0, dt, (float) (handToLocal[0, 2] * Pow(dt, 2) / 2), 0, 0, (float) (handToLocal[1, 2] * Pow(dt, 2) / 2),
                1, dt, (float) (handToLocal[2, 2] * Pow(dt, 2) / 2)
            },
            {0, 0, handToLocal[0, 2] * dt, 0, 0, handToLocal[1, 2] * dt, 0, 1, handToLocal[2, 2] * dt},
            {0, 0, 0, 0, 0, 0, 0, 0, 1}
        });
    }
    
    Matrix<float> rotationMatrixFromQuaternion(float q0, float q1, float q2, float q3)
    {
        // First row of the rotation matrix
        float r00 = 2 * (q0 * q0 + q1 * q1) - 1;
        float r01 = 2 * (q1 * q2 - q0 * q3);
        float r02 = 2 * (q1 * q3 + q0 * q2);

        // Second row of the rotation matrix
        float r10 = 2 * (q1 * q2 + q0 * q3);
        float r11 = 2 * (q0 * q0 + q2 * q2) - 1;
        float r12 = 2 * (q2 * q3 - q0 * q1);

        // Third row of the rotation matrix
        float r20 = 2 * (q1 * q3 - q0 * q2);
        float r21 = 2 * (q2 * q3 + q0 * q1);
        float r22 = 2 * (q0 * q0 + q3 * q3) - 1;

        // 3x3 rotation matrix
        return DenseMatrix.OfArray(new float[3, 3]
        {
            {r00, r01, r02},
            {r10, r11, r12},
            {r20, r21, r22}
        });
    }
}
