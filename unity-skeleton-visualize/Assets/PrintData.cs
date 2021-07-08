using System;
using System.Text;
using UnityEngine;
using UnityEngine.Windows;

public class PrintData : MonoBehaviour
{
    private StringBuilder printText = new StringBuilder(200);
    private KinectManager _manager;
    private DateTime startTime;

    private bool _isTracking;

    public bool Tracking
    {
        set { _isTracking = value; }
    }


    [SerializeField] private SampleUserPolling_ReadWrite _readWrite;
    [SerializeField] private String path;
    [SerializeField] private String fileName;


    // Start is called before the first frame update
    void Start()
    {
        printText.AppendLine(
            "t.m;t.s;t.ms;kinect_foot_right.x;kinect_foot_right.y;kinect_foot_right.z;kinect_foot_left.x;kinect_foot_left.y;" +
            "kinect_foot_left.z;kinect_hand_right.x;kinect_hand_right.y;kinect_hand_right.z;kinect_hand_left.x;" +
            "kinect_hand_left.y;kinect_hand_left.z;imu_acceleration.x;imu_acceleration.y;imu_acceleration.z;" +
            "imu_orientation.x;imu_orientation.y;imu_orientation.z;imu_orientation.w");
        startTime = DateTime.Now;
    }

    // Update is called once per frame
    void Update()
    {
        _manager = KinectManager.Instance;
        if (_isTracking)
        {
            if (_manager && _manager.IsInitialized())
            {
                int milliseconds = DateTime.Now.Subtract(startTime).Milliseconds;
                int seconds = DateTime.Now.Subtract(startTime).Seconds;
                int minutes = DateTime.Now.Subtract(startTime).Minutes;
                Vector3 rightFootPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.FootRight);
                Vector3 leftFootPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.FootLeft);
                Vector3 handRightPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.HandRight);
                Vector3 handLeftPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.HandLeft);

                Vector3 imuAcceleration = _readWrite.orientations.ring.accelerometer.ToVector3Int();
                Quaternion imuOrientation = _readWrite.orientations.ring.gyroscope.ToQuat();

                printText.AppendLine( 
                    $"{minutes};" + $"{seconds};" + $"{milliseconds};" +
                    $"{rightFootPosition.x};{rightFootPosition.y};{rightFootPosition.z};" +
                    $"{leftFootPosition.x};{leftFootPosition.y};{leftFootPosition.z};" +
                    $"{handRightPosition.x};{handRightPosition.y};{handRightPosition.z};" +
                    $"{handLeftPosition.x};{handLeftPosition.y};{handLeftPosition.z};" +
                    $"{imuAcceleration.x};{imuAcceleration.y};{imuAcceleration.z};" +
                    $"{imuOrientation.x};{imuOrientation.y};{imuOrientation.z};{imuOrientation.w}"
                );
            }
        }
    }

    public bool WriteData()
    {
        bool retValue = false;
        try
        {
            if (!Directory.Exists(path))
                Directory.CreateDirectory(path);
            System.IO.File.WriteAllText(path + fileName, printText.ToString());
            retValue = true;
        }
        catch (System.Exception ex)
        {
            retValue = false;
        }

        return retValue;
    }
}