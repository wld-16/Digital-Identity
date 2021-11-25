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
            "t.m;t.s;t.ms;" +
            "kinect_foot_right.x;kinect_foot_right.y;kinect_foot_right.z;" +
            "kinect_foot_left.x;kinect_foot_left.y;kinect_foot_left.z;" +
            "kinect_hand_right.x;kinect_hand_right.y;kinect_hand_right.z;" +
            "kinect_hand_left.x;kinect_hand_left.y;kinect_hand_left.z;" +
            "imu_acceleration.x;imu_acceleration.y;imu_acceleration.z;" +
            "imu_gravity.x;imu_gravity.y;imu_gravity.z;" +
            "imu_orientation.x;imu_orientation.y;imu_orientation.z;imu_orientation.w;" +
            "imu_yaw;imu_pitch;imu_roll;" +
            "kinect_foot_right_orientation.x;kinect_foot_right_orientation.y;kinect_foot_right_orientation.z;kinect_foot_right_orientation.w;" +
            "kinect_foot_left_orientation.x;kinect_foot_left_orientation.y;kinect_foot_left_orientation.z;kinect_foot_left_orientation.w;" +
            "kinect_hand_right_orientation.x;kinect_hand_right_orientation.y;kinect_hand_right_orientation.z;kinect_hand_right_orientation.w;" +
            "kinect_hand_left_orientation.x;kinect_hand_left_orientation.y;kinect_hand_left_orientation.z;kinect_hand_left_orientation.w");
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
                Quaternion rightFootOrientation = _manager.GetJointOrientation(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.FootRight, false);
                Vector3 rightFootPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.FootRight);
                Quaternion leftFootOrientation = _manager.GetJointOrientation(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.FootLeft, false);
                Vector3 leftFootPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.FootLeft);
                Quaternion handRightOrientation = _manager.GetJointOrientation(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.HandRight, false);
                Vector3 handRightPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.HandRight);
                Quaternion handLeftOrientation = _manager.GetJointOrientation(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.HandLeft, false);
                Vector3 handLeftPosition = _manager.GetRawSkeletonJointPos(_manager.GetPlayer1ID(),
                    (int) KinectWrapper.NuiSkeletonPositionIndex.HandLeft);

                Vector3 imuAcceleration = _readWrite.orientations.ring.accelerometer.ToVector3Int();
                Quaternion imuOrientation = _readWrite.orientations.ring.gyroscope.ToQuat();
                Vector3 imuGravity = _readWrite.orientations.ring.gravity.ToVector3();
                Vector3 imuEulerAngles = _readWrite.orientations.ring.gyroscope.ToYawPitchRollVector();

                Debug.Log($"{imuEulerAngles.x};{imuEulerAngles.y};{imuEulerAngles.z};");
                printText.AppendLine( 
                    $"{minutes};" + $"{seconds};" + $"{milliseconds};" +
                    $"{rightFootPosition.x};{rightFootPosition.y};{rightFootPosition.z};" +
                    $"{leftFootPosition.x};{leftFootPosition.y};{leftFootPosition.z};" +
                    $"{handRightPosition.x};{handRightPosition.y};{handRightPosition.z};" +
                    $"{handLeftPosition.x};{handLeftPosition.y};{handLeftPosition.z};" +
                    $"{imuAcceleration.x};{imuAcceleration.y};{imuAcceleration.z};" +
                    $"{imuGravity.x};{imuGravity.y};{imuGravity.z};" +
                    $"{imuOrientation.x};{imuOrientation.y};{imuOrientation.z};{imuOrientation.w};" +
                    $"{imuEulerAngles.x};{imuEulerAngles.y};{imuEulerAngles.z};" +
                    $"{rightFootOrientation.x};{rightFootOrientation.y};{rightFootOrientation.z};{rightFootOrientation.w};" +
                    $"{leftFootOrientation.x};{leftFootOrientation.y};{leftFootOrientation.z};{leftFootOrientation.w};" +
                    $"{handRightOrientation.x};{handRightOrientation.y};{handRightOrientation.z};{handRightOrientation.w};" +
                    $"{handLeftOrientation.x};{handLeftOrientation.y};{handLeftOrientation.z};{handLeftOrientation.w}"
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