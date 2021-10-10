using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using MathNet.Numerics.LinearAlgebra.Double;
using UnityEngine;

public class FetchOrientation : MonoBehaviour
{
    [SerializeField] private KalmanFilter _kalmanFilter;
    [SerializeField] private OrientationSource _orientationSource;

    public SampleUserPolling_ReadWrite readWith;
    public Vector3 fetchedOrienatationAxis;
    public Quaternion fetchedQuaternion = Quaternion.identity;
    public Vector3 fetchedAcceleration;
    public bool applyToTransform;
    public Quaternion orientationOffset;



    [SerializeField] private AxisMappingEntry[] mapping = new AxisMappingEntry[3]
    {
        new AxisMappingEntry(IncomingOrientationAxis.YAW, OutputingOrientationAxis.X),
        new AxisMappingEntry(IncomingOrientationAxis.PITCH, OutputingOrientationAxis.Y),
        new AxisMappingEntry(IncomingOrientationAxis.ROLL, OutputingOrientationAxis.Z)
    };

    // Start is called before the first frame update
    void Start()
    {

    }

    Vector3 accelerometerToAxis(Vector3 accData)
    {
        double phi = -Math.Atan2(accData.y, Math.Sqrt(Math.Pow(accData.x, 2) + Math.Pow(accData.z, 2))) * Mathf.Rad2Deg;
        double theta = Math.Atan2(accData.x, Math.Sqrt(Math.Pow(accData.y, 2) + Math.Pow(accData.z, 2))) *
                       Mathf.Rad2Deg;

        return new Vector3((float) phi, (float) theta, 0);
    }

    // Update is called once per frame
    void Update()
    {
        fetchedOrienatationAxis = readWith.orientations.ringAxis.ToYawPitchRollVector();
        if (readWith.orientations.ring.accelerometer != null)
        {
            fetchedAcceleration = readWith.orientations.ring.accelerometer.ToVector3Int();
        }

        if (readWith.orientations.ring.gyroscope != null)
        {
            fetchedQuaternion = readWith.orientations.ring.gyroscope.ToQuat();
        }

        if (applyToTransform)
        {
            Quaternion outputRotation = Quaternion.identity;
            switch (_orientationSource)
            {
                case OrientationSource.ACCELEROMETER:
                    Vector3 toAxis = accelerometerToAxis(fetchedAcceleration);
                    outputRotation = Quaternion.Euler(mapping.Select(entry => entry.applyMapping(toAxis))
                        .Aggregate((leftVector3, rightVector3) => leftVector3 + rightVector3));
                    break;
                case OrientationSource.GYROSCOPE:
                    outputRotation = fetchedQuaternion;
                    break;
                case OrientationSource.IMUAXIS:
                    Vector3 axis = mapping.Select(entry => entry.applyMapping(fetchedOrienatationAxis))
                        .Aggregate((leftVector3, rightVector3) => leftVector3 + rightVector3);
                    outputRotation = Quaternion.Euler(axis);
                    break;
                case OrientationSource.KALMAN:
                    //output = Quaternion.Euler(mapping.Select(entry => entry.applyMapping(_kalmanFilter.output)).Aggregate((leftVector3, rightVector3) => leftVector3 + rightVector3));
                    break;
            }

            transform.rotation = outputRotation * orientationOffset;
        }
    }
}

[Serializable]
enum IncomingOrientationAxis
{
    PITCH,
    ROLL,
    YAW
}

[Serializable]
enum OrientationSource
{
    ACCELEROMETER,
    GYROSCOPE,
    IMUAXIS,
    KALMAN
}

[Serializable]
enum OutputingOrientationAxis
{
    X,
    Y,
    Z,
    X_NEGATIVE,
    Y_NEGATIVE,
    Z_NEGATIVE,
    ZERO
}

[SerializeField]
enum InputAccelerationAxis
{
    X,
    Y,
    Z
}

[Serializable]
class AxisMappingEntry
{
    [SerializeField] private IncomingOrientationAxis ioa;
    [SerializeField] private OutputingOrientationAxis ooa;

    public AxisMappingEntry(IncomingOrientationAxis ioa, OutputingOrientationAxis ooa)
    {
        this.ioa = ioa;
        this.ooa = ooa;
    }

    public Vector3 applyMapping(Vector3 vector3)
    {
        Vector3 outputVector = new Vector3();

        switch (ioa)
        {
            case IncomingOrientationAxis.YAW:
                switch (ooa)
                {
                    case OutputingOrientationAxis.X:
                        outputVector += new Vector3(vector3.x, 0, 0);
                        break;
                    case OutputingOrientationAxis.Y:
                        outputVector += new Vector3(0, vector3.x, 0);
                        break;
                    case OutputingOrientationAxis.Z:
                        outputVector += new Vector3(0, 0, vector3.x);
                        break;
                    case OutputingOrientationAxis.X_NEGATIVE:
                        outputVector += new Vector3(-vector3.x, 0, 0);
                        break;
                    case OutputingOrientationAxis.Y_NEGATIVE:
                        outputVector += new Vector3(0, -vector3.x, 0);
                        break;
                    case OutputingOrientationAxis.Z_NEGATIVE:
                        outputVector += new Vector3(0, 0, -vector3.x);
                        break;
                }

                break;
            case IncomingOrientationAxis.PITCH:
                switch (ooa)
                {
                    case OutputingOrientationAxis.X:
                        outputVector += new Vector3(vector3.y, 0, 0);
                        break;
                    case OutputingOrientationAxis.Y:
                        outputVector += new Vector3(0, vector3.y, 0);
                        break;
                    case OutputingOrientationAxis.Z:
                        outputVector += new Vector3(0, 0, vector3.y);
                        break;
                    case OutputingOrientationAxis.X_NEGATIVE:
                        outputVector += new Vector3(-vector3.y, 0, 0);
                        break;
                    case OutputingOrientationAxis.Y_NEGATIVE:
                        outputVector += new Vector3(0, -vector3.y, 0);
                        break;
                    case OutputingOrientationAxis.Z_NEGATIVE:
                        outputVector += new Vector3(0, 0, -vector3.y);
                        break;
                }

                break;
            case IncomingOrientationAxis.ROLL:
                switch (ooa)
                {
                    case OutputingOrientationAxis.X:
                        outputVector += new Vector3(vector3.z, 0, 0);
                        break;
                    case OutputingOrientationAxis.Y:
                        outputVector += new Vector3(0, vector3.z, 0);
                        break;
                    case OutputingOrientationAxis.Z:
                        outputVector += new Vector3(0, 0, vector3.z);
                        break;
                    case OutputingOrientationAxis.X_NEGATIVE:
                        outputVector += new Vector3(-vector3.z, 0, 0);
                        break;
                    case OutputingOrientationAxis.Y_NEGATIVE:
                        outputVector += new Vector3(0, -vector3.z, 0);
                        break;
                    case OutputingOrientationAxis.Z_NEGATIVE:
                        outputVector += new Vector3(0, 0, -vector3.z);
                        break;
                }

                break;
        }

        return outputVector;
    }
}