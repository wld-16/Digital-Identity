using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using MathNet.Numerics.LinearAlgebra.Double;
using UnityEngine;

public class FetchOrientation : MonoBehaviour, IPushData
{
    [SerializeField] private OrientationSource _orientationSource;

    public SampleUserPolling_ReadWrite readWith;
    public Quaternion fetchedQuaternion = Quaternion.identity;
    public Vector3 fetchedAcceleration;
    public bool applyToTransform;
    public Quaternion orientationOffset;

    public List<float> outputData;
    public List<IPullData> dataRecipients;


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
        if (readWith.orientations.ring.accelerometer != null)
        {
            fetchedAcceleration = readWith.orientations.ring.accelerometer.ToVector3();
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
                case OrientationSource.QUATERNION:
                    outputRotation = fetchedQuaternion;
                    outputData = new List<float>
                    {
                        fetchedQuaternion.w,
                        fetchedQuaternion.x,
                        fetchedQuaternion.y,
                        fetchedQuaternion.z
                    };
                    break;
                case OrientationSource.YPR:
                    outputData = new List<float>
                    {
                        readWith.orientations.ring.gyroscope.yaw,
                        readWith.orientations.ring.gyroscope.pitch,
                        readWith.orientations.ring.gyroscope.roll
                    };
                    break;
                case OrientationSource.KALMAN:
                    //output = Quaternion.Euler(mapping.Select(entry => entry.applyMapping(_kalmanFilter.output)).Aggregate((leftVector3, rightVector3) => leftVector3 + rightVector3));
                    break;
            }

            transform.rotation = outputRotation * orientationOffset;
        }
    }

    public List<IPullData> getDataRecipients()
    {
        return dataRecipients;
    }

    public void PushData()
    {
        getDataRecipients()
            .ForEach(recipient => recipient.Receive(outputData));
    }

    public List<float> getData()
    {
        return outputData;
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
    QUATERNION,
    YPR,
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