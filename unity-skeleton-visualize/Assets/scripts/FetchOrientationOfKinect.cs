using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class FetchOrientationOfKinect : MonoBehaviour, IPushData
{
    [SerializeField] private List<Component> receivingComponents;

    [SerializeField] private KinectManager _kinectManager;
    [SerializeField] private KinectWrapper.NuiSkeletonPositionIndex _skeletonPositionIndex;

    [SerializeField] private Quaternion outputOrientation = Quaternion.identity;
    [SerializeField] private List<float> outputData;
    

    // Update is called once per frame
    void Update()
    {
        _kinectManager = KinectManager.Instance;
        if (_kinectManager && _kinectManager.IsInitialized() && _kinectManager.IsUserDetected())
        {
            Quaternion kinectOrientation = _kinectManager.GetJointOrientation(_kinectManager.GetPlayer1ID(),
                (int) _skeletonPositionIndex, false);


            if (!kinectOrientation.Equals(Quaternion.identity))
            {
                outputData = new List<float>() {kinectOrientation.w, kinectOrientation.x, kinectOrientation.y, kinectOrientation.z};
                outputOrientation = kinectOrientation;
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