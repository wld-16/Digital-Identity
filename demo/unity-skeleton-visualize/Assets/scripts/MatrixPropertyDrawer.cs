using System.Collections;
using System.Collections.Generic;
using MathNet.Numerics.LinearAlgebra;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;

[CustomPropertyDrawer(typeof(FloatMatrix))]
public class MatrixPropertyDrawer : PropertyDrawer
{
    public bool showPosition = true;
    int dimensions = 2;
    private float basePropertyHeight = 20f;
    

    public override float GetPropertyHeight(SerializedProperty property, GUIContent label)
    {
        return (basePropertyHeight) * (dimensions + 1);
    }

    public override void OnGUI(Rect position, SerializedProperty property, GUIContent label)
    {

        EditorGUI.BeginProperty(position, label, property);


        SerializedProperty dimensionProperty = property.FindPropertyRelative("dimension");
        dimensions = dimensionProperty.intValue;

        SerializedProperty dataProperty = property.FindPropertyRelative("data");
        
        position = EditorGUI.PrefixLabel(position, GUIUtility.GetControlID(FocusType.Passive), label);

        EditorGUIUtility.wideMode = true;
        EditorGUIUtility.labelWidth = 70;
        
        
        dimensionProperty.intValue = EditorGUI.IntField(new Rect(position.position.x,position.position.y, position.size.x,basePropertyHeight),"Dimension", dimensionProperty.intValue);
        EditorGUIUtility.labelWidth = 20;

        if (showPosition)
        {
            for (int row = 0; row < dimensions; row++)
            {
                for (int col = 0; col < dimensions; col++)
                {
                
                    SerializedProperty floatValue = dataProperty.GetArrayElementAtIndex(dimensions * row + col);

                    bool isBiggerThan40 = position.size.x / dimensions - EditorGUIUtility.labelWidth > 40;
                    
                    string labelForVal = isBiggerThan40 ? row + "," + col : "";

                    floatValue.floatValue = EditorGUI.FloatField(
                        new Rect(position.position.x + col * position.size.x / dimensions,
                            position.position.y + row * (position.size.y - basePropertyHeight) / dimensions + basePropertyHeight, position.size.x / dimensions - (isBiggerThan40 ? EditorGUIUtility.labelWidth:0) ,
                            (position.size.y - basePropertyHeight) / dimensions),labelForVal , floatValue.floatValue);
                }
            }
        }


        EditorGUI.EndProperty();
    }
}