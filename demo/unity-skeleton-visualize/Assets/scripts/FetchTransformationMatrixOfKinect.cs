using System.Collections;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Single;
using UnityEngine;

public class FetchTransformationMatrixOfKinect : MonoBehaviour, IPushData
{
    [SerializeField] private List<Component> receivingComponents;

    [SerializeField] private KinectManager _kinectManager;
    [SerializeField] private KinectWrapper.NuiSkeletonPositionIndex _skeletonPositionIndex;

    [SerializeField] private Matrix<float> rotationMatrix = DenseMatrix.CreateIdentity(3);
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
                rotationMatrix = rotationMatrixFromQuaternion(kinectOrientation.w, kinectOrientation.x, kinectOrientation.y, kinectOrientation.z);

                outputData = new List<float>()
                {
                    rotationMatrix[0,0], rotationMatrix[1,0], rotationMatrix[2,0],
                    rotationMatrix[0,1], rotationMatrix[1,1], rotationMatrix[2,1],
                    rotationMatrix[0,2], rotationMatrix[1,2], rotationMatrix[2,2]
                };
            }
        }
    }

    Matrix<float> rotationMatrixFromQuaternion(float q0, float q1, float q2, float q3)
    {
        // First row of the rotation matrix
        float r00 = 2 * (q0 * q0 + q1 * q1) - 1;
        float r01 = 2 * (q1 * q2 - q0 * q3);
        float r02 = 2 * (q1 * q3 + q0 * q2);

        // Second row of the rotation matrix
        float r10 = 2 * (q1 * q2 + q0 * q3);
        float r11 = 2 * (q0 * q0 + q2 * q2) - 1;
        float r12 = 2 * (q2 * q3 - q0 * q1);

        // Third row of the rotation matrix
        float r20 = 2 * (q1 * q3 - q0 * q2);
        float r21 = 2 * (q2 * q3 + q0 * q1);
        float r22 = 2 * (q0 * q0 + q3 * q3) - 1;

        // 3x3 rotation matrix
        return DenseMatrix.OfArray(new float[3, 3]
        {
            {r00, r01, r02},
            {r10, r11, r12},
            {r20, r21, r22}
        });
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