using System;
using System.Collections;
using System.Collections.Generic;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Factorization;
using MathNet.Numerics.LinearAlgebra.Storage;
using UnityEngine;

[Serializable]
public class FloatMatrix
{
    public int dimension;
    public float[] data;

    public FloatMatrix(int dimension, float[] data)
    {
        this.dimension = dimension;
        this.data = data;
    }

    public int Dimension
    {
        get => dimension;
        set => dimension = value;
    }

    public float[] Data
    {
        get => data;
        set => data = value;
    }
}
