using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FetchKinectPosition : MonoBehaviour
{
    [SerializeField] private KinectManager _kinectManager;
    [SerializeField] private KinectWrapper.NuiSkeletonPositionIndex _skeletonPositionIndex;

    [SerializeField] private Vector3 outputPosition;
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
                outputPosition = kinectPosition;
            }
        }

        if (applyToLocalPosition)
        {
            transform.localPosition = outputPosition;
        }
    }
}
