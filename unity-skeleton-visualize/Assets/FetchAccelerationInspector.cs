using UnityEditor;
using UnityEngine;


[CustomEditor(typeof(FetchAcceleration))]
public class FetchAccelerationInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();
        
        FetchAcceleration fetchAcceleration = (FetchAcceleration) target;

        if (GUILayout.Button("Start Calibration"))
        {
            fetchAcceleration.StartCalibration();
        }
        
        if (GUILayout.Button("End Calibration"))
        {
            fetchAcceleration.EndCalibration();
        }
    }
}
