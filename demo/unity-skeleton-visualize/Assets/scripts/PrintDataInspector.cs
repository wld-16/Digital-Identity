
#if UNITY_EDITOR
using UnityEditor;
using UnityEngine;

[CustomEditor(typeof(PrintData))]
public class PrintDataInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();

        PrintData myScript = (PrintData) target;
        if (GUILayout.Button("Print to File"))
        {
            myScript.WriteData();
        }
        if (GUILayout.Button("Start Tracking"))
        {
            myScript.Tracking = true;
        }
        if (GUILayout.Button("Stop Tracking"))
        {
            myScript.Tracking = false;
        }
    }

}
#endif