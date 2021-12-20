using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Random = System.Random;

public class GenerateAccelerometerSensorData : MonoBehaviour, IPushData
{
    public List<IPullData> dataRecipients;
    [SerializeField] private List<float> outputData;

    [SerializeField] public float impulsePower; 
    [SerializeField] private float dt = 0.014f;
    [SerializeField] private float accelerometerSpectralNoiseDensity = Mathf.Pow(10,-6) * 9.81f * 1000;
    // Unit of Noise spectral density in microG/root(Hz) for Accelerometer at 1kHz
    
    

    // Update is called once per frame
    void LateUpdate()
    {
        float randomX = UnityEngine.Random.Range(-accelerometerSpectralNoiseDensity, accelerometerSpectralNoiseDensity) * dt;
        float randomY = UnityEngine.Random.Range(-accelerometerSpectralNoiseDensity, accelerometerSpectralNoiseDensity) * dt;
        float randomZ = UnityEngine.Random.Range(-accelerometerSpectralNoiseDensity, accelerometerSpectralNoiseDensity) * dt;

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
            .ForEach(recipient => recipient.Receive(outputData));
    }

    public List<float> getData()
    {
        return outputData;
    }

    public void GenImpulse(float x, float y, float z)
    {
        outputData[0] += x;
        outputData[1] += y;
        outputData[2] += z;
    }
}