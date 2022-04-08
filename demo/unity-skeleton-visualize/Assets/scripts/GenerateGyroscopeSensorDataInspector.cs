
#if UNITY_EDITOR
using UnityEditor;
using UnityEngine;
using UnityEngine.UI;

[CustomEditor(typeof(GenerateGyroscopeSensorData))]
public class GenerateGyroscopeSensorDataInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();
        GenerateGyroscopeSensorData generator = (GenerateGyroscopeSensorData) target;
        float velocityIncrement = generator.velocityIncrement;

        if (GUILayout.Button("Increment Axis Down"))
        {
            generator.AddVelocityIncrement(0,-velocityIncrement,0);
        }
        if (GUILayout.Button("Increment Axis Up"))
        {
            generator.AddVelocityIncrement(0,velocityIncrement,0);
        }
        if (GUILayout.Button("Increment Axis Right"))
        {
            generator.AddVelocityIncrement(velocityIncrement,0,0);
        }
        if (GUILayout.Button("Increment Axis Left"))
        {
            generator.AddVelocityIncrement(-velocityIncrement,0,0);
        }
        if (GUILayout.Button("Increment Axis Forward"))
        {
            generator.AddVelocityIncrement(0,0,velocityIncrement);
        }
        if (GUILayout.Button("Increment Axis Back"))
        {
            generator.AddVelocityIncrement(0,0,-velocityIncrement);
        }
    }
}
#endif