using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class FetchAccelerometer : MonoBehaviour, IPushData
{
    #region dataSource

    [SerializeField] public SampleUserPolling_ReadWrite readWith;
    [SerializeField] public Vector3 fetchedAcceleration;
    [SerializeField] private Vector3 fetchedGravityVector;

    #endregion

    #region transformations

    [SerializeField] private Vector3 offset;
    [SerializeField] private Vector3 thresholdVector = new Vector3(1, 1, 1);

    #endregion

    #region output 

    [SerializeField] private bool removeGravity = true;
    [SerializeField] public Vector3 rawOutput;
    [SerializeField] public List<float> outputData;
    [SerializeField] private List<IPullData> receipients;

    #endregion

    #region derivedOrIntegratedData

    [SerializeField] private Vector3 cumulativeVelocity = new Vector3(0, 0, 0);
    [SerializeField] private Vector3 cumulativePosition = new Vector3(0, 0, 0);
    [SerializeField] private Vector3 accelerationScale;
    [SerializeField] private Vector3 velocityScale;
    [SerializeField] private Vector3 positionScale;

    #endregion

    [SerializeField] private AccelerationMapping[] accelerometerMapping = new AccelerationMapping[3]
    {
        new AccelerationMapping(InputAccelerationAxis.X, OutputAccelerationAxis.X),
        new AccelerationMapping(InputAccelerationAxis.Y, OutputAccelerationAxis.Y),
        new AccelerationMapping(InputAccelerationAxis.Z, OutputAccelerationAxis.Z)
    };

    private Vector3 calibrationSum;
    private int calibrationCount = 0;
    private bool isCalibrating;


    // Start is called before the first frame update
    void Start()
    {
        outputData = new List<float> {0, 0, 0};
    }

    // Update is called once per frame
    void Update()
    {
        rawOutput = accelerometerMapping.Select(entry =>
                entry.applyMapping(readWith.orientations.ring.accelerometer.ToVector3()))
            .Aggregate((leftVector3, rightVector3) => leftVector3 + rightVector3);
        fetchedGravityVector = readWith.orientations.ring.gravity.ToVector3();

        outputData[0] = rawOutput.x - (removeGravity ? fetchedGravityVector.x * 9.81f : 0);
        outputData[1] = rawOutput.y - (removeGravity ? fetchedGravityVector.y * 9.81f : 0);
        outputData[2] = rawOutput.z - (removeGravity ? fetchedGravityVector.z * 9.81f : 0);

        if (Math.Abs(rawOutput.magnitude) > 0.001f)
        {
            Vector3 tempFetch = rawOutput;
            tempFetch += offset;
            tempFetch.Scale(accelerationScale);

            if (Math.Abs(tempFetch.x) > thresholdVector.x || Math.Abs(tempFetch.y) > thresholdVector.y ||
                Math.Abs(tempFetch.z) > thresholdVector.z)
            {
                Vector3 currentVelocity = tempFetch;
                currentVelocity.Scale(velocityScale);
                cumulativeVelocity = cumulativeVelocity + currentVelocity;

                Vector3 currentPosition = currentVelocity;
                cumulativePosition.Scale(positionScale);

                cumulativePosition = currentPosition;

                transform.localPosition = cumulativePosition;
                fetchedAcceleration = cumulativePosition;
            }
        }
    }

    public List<IPullData> getDataRecipients()
    {
        return receipients;
    }

    public void PushData()
    {
        getDataRecipients().ForEach(recipient =>
            recipient.Receive(outputData));
    }

    public List<float> getData()
    {
        return new List<float>() {rawOutput.x, rawOutput.y, rawOutput.z};
    }
}


enum OutputAccelerationAxis
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
class AccelerationMapping
{
    [SerializeField] private InputAccelerationAxis iaa;
    [SerializeField] private OutputAccelerationAxis oaa;

    public AccelerationMapping(InputAccelerationAxis iaa, OutputAccelerationAxis oaa)
    {
        this.iaa = iaa;
        this.oaa = oaa;
    }

    public Vector3 applyMapping(Vector3 vector3)
    {
        Vector3 outputVector3 = new Vector3();

        switch (iaa)
        {
            case InputAccelerationAxis.X:
                switch (oaa)
                {
                    case OutputAccelerationAxis.X:
                        outputVector3 += new Vector3(vector3.x, 0, 0);
                        break;
                    case OutputAccelerationAxis.Y:
                        outputVector3 += new Vector3(0, vector3.x, 0);
                        break;
                    case OutputAccelerationAxis.Z:
                        outputVector3 += new Vector3(0, 0, vector3.x);
                        break;
                    case OutputAccelerationAxis.X_NEGATIVE:
                        outputVector3 += new Vector3(-vector3.x, 0, 0);
                        break;
                    case OutputAccelerationAxis.Y_NEGATIVE:
                        outputVector3 += new Vector3(0, -vector3.x, 0);
                        break;
                    case OutputAccelerationAxis.Z_NEGATIVE:
                        outputVector3 += new Vector3(0, 0, -vector3.x);
                        break;
                }

                break;
            case InputAccelerationAxis.Y:
                switch (oaa)
                {
                    case OutputAccelerationAxis.X:
                        outputVector3 += new Vector3(vector3.y, 0, 0);
                        break;
                    case OutputAccelerationAxis.Y:
                        outputVector3 += new Vector3(0, vector3.y, 0);
                        break;
                    case OutputAccelerationAxis.Z:
                        outputVector3 += new Vector3(0, 0, vector3.y);
                        break;
                    case OutputAccelerationAxis.X_NEGATIVE:
                        outputVector3 += new Vector3(-vector3.y, 0, 0);
                        break;
                    case OutputAccelerationAxis.Y_NEGATIVE:
                        outputVector3 += new Vector3(0, -vector3.y, 0);
                        break;
                    case OutputAccelerationAxis.Z_NEGATIVE:
                        outputVector3 += new Vector3(0, 0, -vector3.y);
                        break;
                }

                break;
            case InputAccelerationAxis.Z:
                switch (oaa)
                {
                    case OutputAccelerationAxis.X:
                        outputVector3 += new Vector3(vector3.z, 0, 0);
                        break;
                    case OutputAccelerationAxis.Y:
                        outputVector3 += new Vector3(0, vector3.z, 0);
                        break;
                    case OutputAccelerationAxis.Z:
                        outputVector3 += new Vector3(0, 0, vector3.z);
                        break;
                    case OutputAccelerationAxis.X_NEGATIVE:
                        outputVector3 += new Vector3(-vector3.z, 0, 0);
                        break;
                    case OutputAccelerationAxis.Y_NEGATIVE:
                        outputVector3 += new Vector3(0, -vector3.z, 0);
                        break;
                    case OutputAccelerationAxis.Z_NEGATIVE:
                        outputVector3 += new Vector3(0, 0, -vector3.z);
                        break;
                }

                break;
        }

        return outputVector3;
    }
}