using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[Serializable]
public class Orientations
{
    [SerializeField] public Ring ring;
}

public abstract class RawSensorData
{
    public Accelerometer accelerometer;
    public Gyroscope gyroscope;
    public Gravity gravity;
}

[Serializable]
public class Ring
{
    [SerializeField] public Accelerometer accelerometer;
    [SerializeField] public Gyroscope gyroscope;
    [SerializeField] public Gravity gravity;
}

[Serializable]
public class Hand
{
}

[Serializable]
public class Gravity
{
    public float x;
    public float y;
    public float z;

    public Vector3 ToVector3()
    {
        return new Vector3(x, y, z);
    }
}

[Serializable]
public class Accelerometer
{
    public float x;
    public float y;
    public float z;

    public Vector3 ToVector3()
    {
        return new Vector3(x, y, z);
    }
}

[Serializable]
public class Gyroscope
{
    public float yaw;
    public float pitch;
    public float roll;
    public float x;
    public float y;
    public float z;
    public float w;

    public Quaternion ToQuat()
    {
        return new Quaternion(x, y, z, w);
    }
    public Vector3 ToYawPitchRollVector()
    {
        return new Vector3(yaw, pitch, roll);
    }
}