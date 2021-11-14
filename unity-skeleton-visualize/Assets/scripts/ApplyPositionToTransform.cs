﻿using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Serialization;

public class ApplyPositionToTransform : MonoBehaviour, IPullData
{
    [SerializeField] private Component inputDataComponent;
    
    public SOURCE source = SOURCE.ACCELEROMETER;
    
    private FetchKinectPosition fetchKinectPosition;
    private FetchAccelerometer fetchAccelerometer; 
    private GeneralKalmanFilterBehaviour kalmanFilterBehaviour;
    

    public bool applyToLocalPosition = true;
    

    public FetchKinectPosition KinectPosition
    {
        get { return fetchKinectPosition; }
        set { fetchKinectPosition = value; }
    }

    public GeneralKalmanFilterBehaviour KalmanFilter
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
                        PullData();
                    }
                    break;
                case SOURCE.ACCELEROMETER:
                    if (FetchAccelerometer != null)
                    {
                        transform.localPosition = FetchAccelerometer.fetchedAcceleration;
                    }
                    break;
                case SOURCE.KALMAN_FILTER:
                    PullData();
                    break;
            }
        }
    }

    public IPushData getDataDeliverer()
    {
        return (IPushData) inputDataComponent;
    }

    public void PullData()
    {
        List<float> receivedData = getDataDeliverer().getData();
        Debug.Log(receivedData[0]);
        if (receivedData.Count >= 3 && (!float.IsNaN(receivedData[0]) && !float.IsNaN(receivedData[1]) && !float.IsNaN(receivedData[2])))
        {
            transform.localPosition = new Vector3(receivedData[0], receivedData[1], receivedData[2]);
        }
    }

    public void Receive(List<float> receivedData)
    {
        if (receivedData.Count >= 3)
        {
            transform.localPosition = new Vector3(receivedData[0], receivedData[1], receivedData[2]);
        }
    }
}

public enum SOURCE {
    KINECT,
    ACCELEROMETER,
    KALMAN_FILTER
} 