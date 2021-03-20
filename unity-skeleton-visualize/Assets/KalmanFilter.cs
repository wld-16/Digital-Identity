using System;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class KalmanFilter : MonoBehaviour
{
    public FetchOrientation fetchOrientation;
    
    public Vector3 accelerometerData;
    public Quaternion gyroData;
    public Quaternion gyroOffset;

    public Vector3 output;
    
    private Matrix4x4 C;
    private Matrix4x4 P;
    private Matrix4x4 Q;
    private Matrix4x4 R;

    [SerializeField] private Vector4 stateEstimate;
    private int N;
    [SerializeField] private float phi_hat;
    [SerializeField] private float theta_hat;

    private Matrix4x4 A;
    private Matrix4x4 B;

    private void Start()
    {
        C = new Matrix4x4(new Vector4(1,0,0,0), new Vector4(0,0,1,0), Vector4.zero, Vector4.zero );
        P = Matrix4x4.identity;
        Q = Matrix4x4.identity;
        R = new Matrix4x4(new Vector4(0.1f,0,0,0),
            new Vector4(0,0.1f,0,0),
            new Vector4(0,0,0.1f,0),
            new Vector4(0,0,0,0.1f));
        
        stateEstimate = Vector4.zero;

        N = 100;
    }

    private void Update()
    {
        Vector2 accAngles = accelerometerToAngles(fetchOrientation.fetchedAcceleration);
        gyroData = gyroOffset * fetchOrientation.fetchedQuaternion;
        accelerometerData = new Vector3(accAngles.x, accAngles.y, gyroData.eulerAngles.z);

        Vector2 kalmanData = KalmanUpdate(gyroData.eulerAngles, accelerometerData);
        output = new Vector3(kalmanData.x, kalmanData.y, gyroData.eulerAngles.z);
    }


    private Vector2 KalmanUpdate(Vector3 gyroInput, Vector3 accInput)
    {
        
        stateEstimate = A.MultiplyVector(stateEstimate) + B.MultiplyVector(gyroInput);
        P = matAddition(A * (P * (A.transpose)), Q);
        
        Vector4 measurement = new Vector4(accInput.x, 0,accInput.y,0);
        Vector4 yTilde = measurement - C * stateEstimate;
        
        Matrix4x4 S = matAddition(R, (C * (P * C.transpose)));
        Matrix4x4 K = P * (C.transpose * S.inverse);

        
        stateEstimate += K * yTilde;
        P = (matSubtraction(Matrix4x4.identity, K) * C) * P;

        phi_hat = stateEstimate.x;
        theta_hat = stateEstimate.y;

        return new Vector2(phi_hat, theta_hat);
    }

    private Matrix4x4 matAddition(Matrix4x4 mat1, Matrix4x4 mat2)
    {
        Matrix4x4 outputMatrix = new Matrix4x4(
                mat1.GetColumn(0) + mat2.GetColumn(0),
                mat1.GetColumn(1) + mat2.GetColumn(1),
                mat1.GetColumn(2) + mat2.GetColumn(2),
                mat1.GetColumn(3) + mat2.GetColumn(3)
        );
        return outputMatrix;
    }
    
    private Matrix4x4 matSubtraction(Matrix4x4 mat1, Matrix4x4 mat2)
    {
        Matrix4x4 outputMatrix = new Matrix4x4(
            mat1.GetColumn(0) + mat2.GetColumn(0),
            mat1.GetColumn(1) + mat2.GetColumn(1),
            mat1.GetColumn(2) + mat2.GetColumn(2),
            mat1.GetColumn(3) + mat2.GetColumn(3)
        );
        return outputMatrix;
    }
    
    Vector3 accelerometerToAngles(Vector3 accData)
    {
        double phi = -Math.Atan2(accData.y, Math.Sqrt(Math.Pow(accData.x, 2) + Math.Pow(accData.z, 2))) * Mathf.Rad2Deg;
        double theta = Math.Atan2(accData.x, Math.Sqrt(Math.Pow(accData.y, 2) + Math.Pow(accData.z, 2))) * Mathf.Rad2Deg;
        
        return new Vector3((float) phi, (float) theta, 0); 
    }
}
