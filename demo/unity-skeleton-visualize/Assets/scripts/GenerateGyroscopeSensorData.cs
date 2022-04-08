using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Random = System.Random;

public class GenerateGyroscopeSensorData : MonoBehaviour, IPushData
{
    public List<IPullData> dataRecipients;
    [SerializeField] private List<float> outputData;

    [SerializeField] public float velocityIncrement = 1;
    [SerializeField] public List<float> velocityMemory;
    [SerializeField] private float dt = 0.014f;

    // Spectral Nosie Density at 10 Hz °/s 
    [SerializeField] private float gyroscopeSpectralNoiseDensity = 0.005f * 10;

    private void Start()
    {
        velocityMemory = outputData.Select(f => 0f).ToList();
    }

    // Update is called once per frame
    void Update()
    {
        float randomX = UnityEngine.Random.Range(-gyroscopeSpectralNoiseDensity, gyroscopeSpectralNoiseDensity);
        float randomY = UnityEngine.Random.Range(-gyroscopeSpectralNoiseDensity, gyroscopeSpectralNoiseDensity);
        float randomZ = UnityEngine.Random.Range(-gyroscopeSpectralNoiseDensity, gyroscopeSpectralNoiseDensity);

        var position = new Vector3(randomX, randomY, randomZ);
        outputData = new List<float>() {position.x, position.y, position.z};
    }

    public List<IPullData> getDataRecipients()
    {
        return dataRecipients;
    }

    public void PushData()
    {
        getDataRecipients()
            .ForEach(
                recipient => recipient.Receive(outputData.Select((f, index) => f + velocityMemory[index]).ToList()));
    }

    public List<float> getData()
    {
        return outputData.Select((f, index) => f + velocityMemory[index]).ToList();
    }

    public void AddVelocityIncrement(float x, float y, float z)
    {
        velocityMemory[0] += x;
        velocityMemory[1] += y;
        velocityMemory[2] += z;
    }
}