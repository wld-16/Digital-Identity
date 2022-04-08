
#if UNITY_EDITOR
using UnityEditor;
using UnityEngine;


[CustomEditor(typeof(KalmanFilterBehaviour))]
public class KalmanFilterBehaviourInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();

        KalmanFilterBehaviour myScript = (KalmanFilterBehaviour) target;
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
#endif