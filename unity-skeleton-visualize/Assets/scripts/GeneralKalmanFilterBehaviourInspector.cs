using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics;
using MathNet.Numerics.Distributions;
using UnityEditor;
using UnityEngine;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Single;
using static MathNet.Numerics.SpecialFunctions;
using static System.Math;
using Vector = MathNet.Numerics.LinearAlgebra.Double.Vector;

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