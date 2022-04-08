
#if UNITY_EDITOR

using UnityEditor;
using UnityEngine;
using UnityEngine.UI;

[CustomEditor(typeof(GenerateAccelerometerSensorData))]
public class GenerateAccelerometerSensorDataInspector : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();
        GenerateAccelerometerSensorData generator = (GenerateAccelerometerSensorData) target;
        float impulsePower = generator.impulsePower;

        if (GUILayout.Button("Impulse Down"))
        {
            generator.GenImpulse(0,-impulsePower,0);
        }
        if (GUILayout.Button("Impulse Up"))
        {
            generator.GenImpulse(0,impulsePower,0);
        }
        if (GUILayout.Button("Impulse Right"))
        {
            generator.GenImpulse(impulsePower,0,0);
        }
        if (GUILayout.Button("Impulse Left"))
        {
            generator.GenImpulse(-impulsePower,0,0);
        }
        if (GUILayout.Button("Impulse Forward"))
        {
            generator.GenImpulse(0,0,impulsePower);
        }
        if (GUILayout.Button("Impulse Back"))
        {
            generator.GenImpulse(0,0,-impulsePower);
        }
    }
}

#endif