using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ApplyAccelerationAtRigidbody : MonoBehaviour, IPullData
{
    [SerializeField] private Component inputDataComponent;
    [SerializeField] private Rigidbody applyRigidbody;

    private void Update()
    {
        PullData(inputDataComponent.GetType());
    }

    public IPushData getDataDeliverer(Type type)
    {
        return (IPushData) inputDataComponent;
    }

    public void PullData(Type type)
    {
        List<float> receivedData = getDataDeliverer(type).getData();
        if (receivedData.Count >= 3 && (!float.IsNaN(receivedData[0]) && !float.IsNaN(receivedData[1]) &&
                                        !float.IsNaN(receivedData[2])))
        {
            applyRigidbody.AddForce(receivedData[0], receivedData[1], receivedData[2], ForceMode.Acceleration);
        }
    }

    public void Receive(List<float> receivedData)
    {
        if (receivedData.Count >= 3)
        {
            applyRigidbody.AddForce(receivedData[0], receivedData[1], receivedData[2], ForceMode.Acceleration);
        }
    }
}