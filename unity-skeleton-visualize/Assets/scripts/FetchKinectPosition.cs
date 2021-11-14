using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class FetchKinectPosition : MonoBehaviour, IPushData
{
    [SerializeField] private List<Component> receivingComponents;

    [SerializeField] private KinectManager _kinectManager;
    [SerializeField] private KinectWrapper.NuiSkeletonPositionIndex _skeletonPositionIndex;

    [SerializeField] private Vector3 outputPosition;
    [SerializeField] private List<float> outputData;
    [SerializeField] private bool applyToLocalPosition = false;

    public Vector3 OutputPosition => outputPosition;

    // Update is called once per frame
    void Update()
    {
        _kinectManager = KinectManager.Instance;
        if (_kinectManager && _kinectManager.IsInitialized() && _kinectManager.IsUserDetected())
        {
            Vector3 kinectPosition = _kinectManager.GetRawSkeletonJointPos(_kinectManager.GetPlayer1ID(),
                (int) _skeletonPositionIndex);


            if (kinectPosition.magnitude > 0.001f)
            {
                outputData = new List<float>() {kinectPosition.x, kinectPosition.y, kinectPosition.z};
                outputPosition = kinectPosition;
            }
        }
    }

    public List<IPullData> getDataRecipients()
    {
        return receivingComponents.Select(receiver => (IPullData) receiver).ToList();
    }

    public void PushData()
    {
        receivingComponents.ForEach(component => ((IPullData) component).Receive(outputData));
    }

    public List<float> getData()
    {
        return outputData;
    }
}