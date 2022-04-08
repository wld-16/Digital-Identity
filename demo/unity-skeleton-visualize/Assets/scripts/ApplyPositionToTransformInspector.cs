using UnityEditor;
using UnityEngine;

[CustomEditor(typeof(ApplyPositionToTransform))]
public class ApplyPositionToTransformInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();
        ApplyPositionToTransform applier = (ApplyPositionToTransform) target;
        
        switch (applier.source)
        {
            case SOURCE.KINECT:
                applier.KinectPosition = (FetchKinectPosition) EditorGUILayout.ObjectField("Fetch Kinect",
                    applier.KinectPosition, typeof(FetchKinectPosition), false);
                applier.KinectPosition = applier.GetComponent<FetchKinectPosition>();
                break;
            case SOURCE.KALMAN_FILTER:
                applier.KalmanFilter = (KalmanFilterBehaviour) EditorGUILayout.ObjectField("Kalman Filter",
                    applier.KalmanFilter, typeof(KalmanFilterBehaviour), false);
                applier.KalmanFilter = applier.GetComponent<KalmanFilterBehaviour>();
                break;
            case SOURCE.ACCELEROMETER:
                applier.FetchAccelerometer = (FetchAccelerometer) EditorGUILayout.ObjectField("Fetch Accelerometer",
                    applier.FetchAccelerometer, typeof(FetchAccelerometer), false);
                applier.FetchAccelerometer = applier.GetComponent<FetchAccelerometer>();
                break;
        }
    }
}