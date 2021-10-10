using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Serialization;

public class ApplyPositionToTransform : MonoBehaviour
{
    public SOURCE source = SOURCE.ACCELEROMETER;
    
    private FetchKinectPosition fetchKinectPosition;
    private FetchAccelerometer fetchAccelerometer; 
    private KalmanFilterBehaviour kalmanFilterBehaviour;

    public bool applyToLocalPosition = true;
    

    public FetchKinectPosition KinectPosition
    {
        get { return fetchKinectPosition; }
        set { fetchKinectPosition = value; }
    }

    public KalmanFilterBehaviour KalmanFilter
    {
        get { return kalmanFilterBehaviour; }
        set { kalmanFilterBehaviour = value; }
    }

    public FetchAccelerometer FetchAccelerometer
    {
        get { return fetchAccelerometer; }
        set { fetchAccelerometer = value; }
    }

    // Start is called before the first frame update
    void Start()
    {
    }

    // Update is called once per frame
    void Update()
    {
        if (applyToLocalPosition)
        {
            switch (source)
            {
                case SOURCE.KINECT:
                    if (KinectPosition != null)
                    {
                        transform.localPosition = KinectPosition.OutputPosition;
                    }
                    break;
                case SOURCE.ACCELEROMETER:
                    if (FetchAccelerometer != null)
                    {
                        transform.localPosition = FetchAccelerometer.fetchedAcceleration;
                    }
                    break;
                case SOURCE.KALMAN_FILTER:
                    if (KalmanFilter != null && (!float.IsNaN(KalmanFilter.output.x) && !float.IsNaN(KalmanFilter.output.y) && !float.IsNaN(KalmanFilter.output.z)))
                    {
                        transform.localPosition = KalmanFilter.output;   
                    }
                    break;
            }
        }
    }
}

public enum SOURCE {
    KINECT,
    ACCELEROMETER,
    KALMAN_FILTER
} 