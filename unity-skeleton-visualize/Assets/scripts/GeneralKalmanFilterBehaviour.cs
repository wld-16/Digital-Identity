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
using TMPro.EditorUtilities;
using static MathNet.Numerics.SpecialFunctions;
using static System.Math;
using Vector = MathNet.Numerics.LinearAlgebra.Double.Vector;

public class GeneralKalmanFilterBehaviour : MonoBehaviour
{
    #region inputData

    [Header("Input data")] [SerializeField]
    private FetchAccelerometer fetchAccelerometer;

    [SerializeField] private FetchKinectPosition fetchKinectPosition;

    #endregion

    #region outData

    [Header("Output data")] [SerializeField]
    private Vector3 dataOffset = new Vector3(0, 0, 0);

    [SerializeField] private Vector3 dataScaleFactors = new Vector3(1, 1, 1);
    [SerializeField] private Vector3 afterScaleVector = Vector3.one;
    [SerializeField] private int[] observationOutputMapping = new int[3];
    [SerializeField] private List<float> kalmanResultX;
    [SerializeField] private List<float> kalmanResultX2;
    [SerializeField] private bool normalizeOutput;
    public Vector3 output;

    #endregion

    #region generalKalmanData

    [Header("Kalman params")] public FloatDictionary sigmaValues;
    public List<float> x;
    public FloatMatrix P;
    public FloatMatrix F;
    public FloatMatrix H;
    public float dt = 14.0f / 1000.0f;
    public int N = 100;
    [SerializeField] private double mean = 0;
    [SerializeField] private double stdDev = 2.56;
    private KalmanFilter kalmanFilter;
    public KalmanFilter KalmanFilter => kalmanFilter;

    // Used for sensor fusion
    [SerializeField] [Range(0, 1)] private float kinectFactor = 0.5f;
    private float formerKinectFactor;
    [SerializeField] [Range(0, 1)] private float imuFactor = 0.5f;
    private float formerImuFactor;

    // Used for blending on reset
    private KalmanFilter kalmanFilter2;

    #endregion

    #region blendVariables

    [Header("Blending")] [SerializeField] private bool useBlendByLikelyHood = false;
    float blendIncrement = 0.01f;
    float blendA = 1.0f;
    float blendB = 0.0f;
    bool blendBetweenFilters = false;
    bool useFirstKalman = true;
    [SerializeField] private float resetThreshold = -500000000;

    #endregion blendVariables


    public void ResetOffset()
    {
        dataOffset = Vector3.zero;
    }

    public void SetCoordinateOffset()
    {
        dataOffset = transform.localPosition;
    }

    private KalmanFilter initKalman(KalmanFilter kalmanFilter)
    {
        kalmanFilter.F = DenseMatrix.OfArray(new float[9, 9]
        {
            {1, 0, 0, dt, 0, 0, dt * dt / 2, 0, 0},
            {0, 0, 0, 1, 0, 0, dt, 0, 0},
            {0, 0, 0, 0, 0, 0, 1, 0, 0},
            {0, 1, 0, 0, dt, 0, 0, dt * dt / 2, 0},
            {0, 0, 0, 0, 1, 0, 0, dt, 0},
            {0, 0, 0, 0, 0, 0, 0, 1, 0},
            {0, 0, 1, 0, 0, dt, 0, 0, dt * dt / 2},
            {0, 0, 0, 0, 0, 1, 0, 0, dt},
            {0, 0, 0, 0, 0, 0, 0, 0, 1}
        });
        
        
        float[][] nestedArrayF = new float[F.dimension][];
        for (int i = 0; i < F.dimension; i++)
        {
            nestedArrayF[i] = F.data.ToList().GetRange(i * F.dimension, F.dimension).ToArray();
        }

        kalmanFilter.F = DenseMatrix.OfRowArrays(nestedArrayF);

        kalmanFilter.x = DenseMatrix.OfArray(new float[9, 1]
        {
            {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}
        });

        float[][] nestedP = new float[F.dimension][];
        for (int row = 0; row < P.dimension; row++)
        {
            nestedP[row] = P.data.ToList().GetRange(row * P.dimension, P.dimension).ToArray();
        }
        
        kalmanFilter.P = DenseMatrix.CreateDiagonal(9, 9, (i) => new float[9] {1f, 1, 1, 1f, 1, 1, 1, 1, 1}[i]);
        

        float[][] nestedArrayH = new float[H.dimension][];
        for (int i = 0; i < H.dimension; i++)
        {
            nestedArrayH[i] = H.data.ToList().GetRange(i * H.dimension, H.dimension).ToArray();
        }
        
        kalmanFilter.H = DenseMatrix.OfRowArrays(nestedArrayH);

        mean = 0;
        stdDev = 2.56;
        kalmanFilter.Q = DenseMatrix.CreateRandom(9, 9, new Normal(mean, stdDev));

        kalmanFilter.R = DenseMatrix.CreateDiagonal(9, 9,
            (i) => (float) Math.Pow(sigmaValues.Select(pair => pair.Value).ToList()[i], 2));


        kalmanFilter.N = 100;
        return kalmanFilter;
    }

    private void Start()
    {
        kalmanFilter = initKalman(kalmanFilter);

        if (useBlendByLikelyHood)
        {
            blendIncrement = 0.01f;
            blendA = 1.0f;
            blendB = 0.0f;
            blendBetweenFilters = false;
            useFirstKalman = true;
            kalmanFilter2 = initKalman(kalmanFilter2);
        }
    }

    private Vector3 approximateDifferential(Vector3 currentVector, Vector3 vectorBefore)
    {
        return (currentVector - vectorBefore) / dt;
    }

    private Vector3 approximateIntegral(Vector3 currentVector3, Vector3 vectorBefore)
    {
        return (currentVector3 - vectorBefore) * dt;
    }

    private void Update()
    {

        float x = 0;
        float y = 0;
        float z = 0;
        float vx = 0;
        float vy = 0;
        float vz = 0;
        float ax = 0;
        float ay = 0;
        float az = 0;


        Matrix<float> Z = DenseMatrix.OfArray(new float[9, 1]
        {
            {x},
            {y},
            {z},
            {vx},
            {vy},
            {vz},
            {ax},
            {ay},
            {az}
        });


        if (useBlendByLikelyHood)
        {
            Dictionary<string, Matrix<float>> kalmanPredict = KalmanPredict(kalmanFilter);
            Dictionary<string, Matrix<float>> kalmanPredict2 = KalmanPredict(kalmanFilter2);

            //transform.position = new Vector3(kalmanPredict["x"].At(0,0) * N,kalmanPredict["x"].At(1,0) * N,kalmanPredict["x"].At(2,0) * N);
            kalmanResultX = kalmanPredict["x"].ToRowMajorArray().ToList();
            kalmanResultX2 = kalmanPredict["x"].ToRowMajorArray().ToList();


            kalmanFilter = KalmanUpdate(Z, kalmanFilter);
            kalmanFilter2 = KalmanUpdate(Z, kalmanFilter2);


            if (blendBetweenFilters)
            {
                if (useFirstKalman)
                {
                    blendA = blendA + blendIncrement;
                    blendB = blendB - blendIncrement;
                    if (blendA == 1)
                    {
                        blendBetweenFilters = false;
                    }
                }
                else
                {
                    blendA = blendA - blendIncrement;
                    blendB = blendB + blendIncrement;
                    if (blendB == 1)
                    {
                        blendBetweenFilters = false;
                    }
                }
            }

            if (kalmanFilter.L < resetThreshold && useFirstKalman)
            {
                Debug.Log("Likelyhood: " + kalmanFilter.L);
                Debug.Log("Crossed Reset Threshold");
                kalmanFilter2 = initKalman(kalmanFilter2);
                kalmanFilter2.x = kalmanFilter.x;
                useFirstKalman = false;
                blendBetweenFilters = true;
            }
            else if (kalmanFilter2.L < resetThreshold && !useFirstKalman)
            {
                Debug.Log("Likelyhood_2: " + kalmanFilter2.L);
                Debug.Log("Crossed Reset Threshold");
                kalmanFilter = initKalman(kalmanFilter);
                useFirstKalman = true;
                blendBetweenFilters = true;
            }


            Vector3 transformPosition = ((new Vector3(
                (kalmanResultX2[observationOutputMapping[0]] * blendA +
                 kalmanResultX2[observationOutputMapping[0]] * blendB) * dataScaleFactors.x,
                (kalmanResultX2[observationOutputMapping[1]] * blendA +
                 kalmanResultX2[observationOutputMapping[1]] * blendB) * dataScaleFactors.y,
                (kalmanResultX2[observationOutputMapping[2]] * blendA +
                 kalmanResultX2[observationOutputMapping[2]] * blendB) * dataScaleFactors.z)));

            Debug.Log("Likelyhood: " + kalmanFilter.L);
            Debug.Log("Likelyhood_2: " + kalmanFilter2.L);

            transformPosition.Scale(afterScaleVector);
            output = transformPosition - dataOffset;
        }
        else
        {
            kalmanFilter = KalmanUpdate(Z, kalmanFilter);

            Dictionary<string, Matrix<float>> kalmanPredict = KalmanPredict(kalmanFilter);

            //transform.position = new Vector3(kalmanPredict["x"].At(0,0) * N,kalmanPredict["x"].At(1,0) * N,kalmanPredict["x"].At(2,0) * N);
            kalmanResultX = kalmanPredict["x"].ToRowMajorArray().ToList();

            Vector3 transformPosition = ((new Vector3(kalmanResultX[observationOutputMapping[0]] * dataScaleFactors.x,
                kalmanResultX[observationOutputMapping[1]] * dataScaleFactors.y,
                kalmanResultX[observationOutputMapping[2]] * dataScaleFactors.z)));

            if (useBlendByLikelyHood)
            {
                Debug.Log("Likelyhood: " + kalmanFilter.L);
            }

            transformPosition.Scale(afterScaleVector);
            output = transformPosition - dataOffset;
        }
    }


    private KalmanFilter KalmanUpdate(Matrix<float> z, KalmanFilter kalmanFilter)
    {
        Matrix<float> S = kalmanFilter.H * kalmanFilter.P * kalmanFilter.H.Transpose() + kalmanFilter.R;
        Matrix<float> K = kalmanFilter.P * kalmanFilter.H.Transpose() * S.Inverse();

        Matrix<float> y = z - kalmanFilter.H * kalmanFilter.x;
        Matrix<float> x = kalmanFilter.x + K * y;
        Matrix<float> P = (DenseMatrix.CreateIdentity(9) - K * kalmanFilter.H) * kalmanFilter.P;

        // Likelyhood function
        double L = LikelyHoodFunction(y.Column(0), (kalmanFilter.H * x).Column(0), S);

        kalmanFilter.K = K;
        kalmanFilter.P = P;
        kalmanFilter.x = x;
        kalmanFilter.L = L;

        return kalmanFilter;
    }

    private double LikelyHoodFunction(Vector<float> x, Vector<float> mu, Matrix<float> S)
    {
        Vector<float> ss = x - mu;

        float z = (ss.ToRowMatrix().Multiply(S.Inverse())).Multiply(ss).Sum();
        int k = S.RowCount;

        return (-0.5 * (k * Log(2 * PI) + Log(S.Determinant())) - (z * 0.5));
    }

    private Dictionary<String, Matrix<float>> KalmanPredict(KalmanFilter kalmanFilter)
    {
        Matrix<float> x = kalmanFilter.F * kalmanFilter.x;
        Matrix<float> P = kalmanFilter.F * kalmanFilter.P * kalmanFilter.F.Transpose() + kalmanFilter.Q;

        Dictionary<String, Matrix<float>> ret = new Dictionary<string, Matrix<float>>();
        ret["x"] = x;
        ret["P"] = P;

        return ret;
    }

    public void ResetFilter(KalmanFilter kalmanFilter)
    {
        initKalman(kalmanFilter);
    }

    private void OnValidate()
    {
        if (F.dimension * F.dimension != F.data.Length)
        {
            float[] newFloat = new float[F.dimension * F.dimension];
            F.data = newFloat;
        }
        if (H.dimension * H.dimension != H.data.Length)
        {
            float[] newFloat = new float[H.dimension * H.dimension];
            H.data = newFloat;
        }
        if (P.dimension * P.dimension != P.data.Length)
        {
            float[] newFloat = new float[P.dimension * P.dimension];
            P.data = newFloat;
        }
        if (Abs(formerImuFactor - imuFactor) > 0.0001)
        {
            kinectFactor = 1 - imuFactor;
        }

        if (Abs(formerKinectFactor - kinectFactor) > 0.0001)
        {
            imuFactor = 1 - kinectFactor;
        }

        formerImuFactor = imuFactor;
        formerKinectFactor = kinectFactor;
    }
}