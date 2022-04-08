using UnityEditor;
using UnityEngine;

[CustomEditor(typeof(GeneralKalmanFilterBehaviour))]
public class GeneralKalmanFilterBehaviourInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();
        
        

        GeneralKalmanFilterBehaviour myScript = (GeneralKalmanFilterBehaviour) target;
        if (GUILayout.Button("Set Offset"))
        {
            myScript.SetCoordinateOffset();
        }

        if (GUILayout.Button("Reset Offset"))
        {
            myScript.ResetOffset();
        }
        
        if (GUILayout.Button("Reset Filter"))
        {
            myScript.ResetFilter(myScript.KalmanFilter);
        }
    }
}