using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FetchAcceleration : MonoBehaviour
{
    [SerializeField] public SampleUserPolling_ReadWrite readWith;
    [SerializeField] public Vector3 fetchedAcceleration;

    [SerializeField] private bool applyToTransform;
    [SerializeField] private Vector3 offset;
    
    private Vector3 calibrationSum;
    private int calibrationCount = 0;
    private bool isCalibrating;
    
    
    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        fetchedAcceleration = readWith.orientations.ring.accelerometer.ToVector3Int();


        if (isCalibrating)
        {
            calibrationSum += fetchedAcceleration;
            calibrationCount++;
        }
        
        if (applyToTransform)
        {
            transform.Translate(Vector3.ClampMagnitude(Time.deltaTime * (fetchedAcceleration - offset),0.01f));
        }
    }

    public void StartCalibration()
    {
        isCalibrating = true;
    }
    
    public void EndCalibration()
    {
        isCalibrating = false;
        offset = calibrationSum / calibrationCount;
        calibrationCount = 0;
        calibrationSum = Vector3.zero;
    }
    
    
}
