using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;


public abstract class Axis
{
    public float yaw;
    public float pitch;
    public float roll;

    public Vector3 ToYawPitchRollVector()
    {
        return new Vector3(yaw,pitch,roll);
    }
}

[Serializable]
public class RingAxis : Axis
{
}

[Serializable]
public class HandAxis : Axis
{}

[Serializable]
public class Orientations
{
    [SerializeField]
    public RingAxis ringAxis;
    [SerializeField]
    public Ring ring;
}

public abstract class RawSensorData
{
    public Accelerometer accelerometer;
    public Gyroscope gyroscope;
}

[Serializable]
public class Ring 
{
    [SerializeField]
    public Accelerometer accelerometer;
    [SerializeField]
    public Gyroscope gyroscope;
}

[Serializable]
public class Hand
{}

[Serializable]
public class Accelerometer
{
    public int x;
    public int y;
    public int z;

    public Vector3Int ToVector3Int()
    {
        return new Vector3Int(x,y,z);
    }
}

[Serializable]
public class Gyroscope
{
    public float x;
    public float y;
    public float z;
    public float w;

    public Quaternion ToQuat()
    {
        return new Quaternion(x,y,z,w);
    }
}