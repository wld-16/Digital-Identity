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

public class KalmanFilterBehaviour : MonoBehaviour
{
    #region inputData
    
    [SerializeField] private FetchAccelerometer fetchAccelerometer;
    [SerializeField] private FetchKinectPosition fetchKinectPosition;
    
    #endregion
    
    #region kinectSpecificVariables
    [SerializeField] private bool willProcessKinectData = false;
    [SerializeField] private Vector3 kinectPosition;
    [SerializeField] private bool measureVelocityOnKinect;
    [SerializeField] private bool measureAccelerationOnKinect;
    [SerializeField] private Vector3 formerDiffVelocity = Vector3.one;
    
    #endregion
    
    #region imuSpecificVariables
    
    public Vector3 accelerometerData;
    [SerializeField] private bool willProcessAccelerometerData = false;
    public Vector3 accumulatedVelocity = Vector3.zero;
    [SerializeField] private bool accumulatePositionOnImu = false;
    [SerializeField] private bool accumulateVelocityOnImu = false;
    private Vector3 formerImuVelocity = Vector3.zero;
    [SerializeField] private Vector3 accumulatedImuVelocity = Vector3.zero;
    [SerializeField] private Vector3 accumulatedImuPosition = Vector3.zero;

    #endregion

    #region outData

    [SerializeField] private Vector3 dataOffset = new Vector3(0, 0, 0);
    [SerializeField] private Vector3 dataScaleFactors = new Vector3(1, 1, 1);
    [SerializeField] private Vector3 afterScaleVector = Vector3.one;
    [SerializeField] private int[] observationOutputMapping = new int[3];
    public Vector3 output;
    [SerializeField] private List<float> kalmanResultX;
    [SerializeField] private List<float> kalmanResultX2;

    #endregion

    #region generalKalmanData

    public FloatDictionary sigmaValues;
    public float dt = 14.0f / 1000.0f;
    public int N = 100;
    private KalmanFilter kalmanFilter;
    public KalmanFilter KalmanFilter => kalmanFilter;
    [SerializeField] private double mean = 0;
    [SerializeField] private double stdDev = 2.56;

    // Used for sensor fusion
    [SerializeField] [Range(0, 1)] private float kinectFactor = 0.5f;
    private float formerKinectFactor;
    [SerializeField] [Range(0, 1)] private float imuFactor = 0.5f;
    private float formerImuFactor;
    
    // Used for blending on reset
    private KalmanFilter kalmanFilter2;
    
    #endregion

    #region historicallRelevantVariables

    [SerializeField] private float phi_hat;
    [SerializeField] private float theta_hat;

    #endregion
    

    #region blendVariables

    [SerializeField] private bool useBlendByLikelyHood = false;
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

        kalmanFilter.x = DenseMatrix.OfArray(new float[9, 1]
        {
            {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}
        });

        kalmanFilter.P = DenseMatrix.CreateDiagonal(9, 9, (i) => new float[9] {1, 1, 1, 1, 1, 1, 1, 1, 1}[i]);

        kalmanFilter.H = DenseMatrix.OfArray(new float[9, 9]
        {
            {0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0},
            {1, 0, 1, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 1, 0, 1, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 1, 0, 1}
        });

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

        float ax = 0;
        float ay = 0;
        float az = 0;

        if (willProcessAccelerometerData)
        {
            ax = fetchAccelerometer.rawOutput.x;
            ay = fetchAccelerometer.rawOutput.y;
            az = fetchAccelerometer.rawOutput.z;

            if (fetchAccelerometer.rawOutput.magnitude > 0.01f)
            {
                if (accumulateVelocityOnImu)
                {
                    Vector3 imuVelocity = approximateIntegral(fetchAccelerometer.rawOutput, accelerometerData);

                    accumulatedImuVelocity += imuVelocity;

                    if (accumulatePositionOnImu)
                    {
                        Vector3 imuPosition = approximateIntegral(imuVelocity, formerImuVelocity);

                        accumulatedImuPosition += imuPosition;
                        formerImuVelocity = imuVelocity;
                    }
                }

                accelerometerData = fetchAccelerometer.rawOutput;
                
            }
        }

        float kinectVx = 0;
        float kinectVy = 0;
        float kinectVz = 0;

        Vector3 pos = kinectPosition;
        Vector3 diffAcceleration = Vector3.one;

        if (willProcessKinectData)
        {
            if (fetchKinectPosition.OutputPosition.magnitude > 0.001f)
            {
                if (measureVelocityOnKinect)
                {
                    Vector3 diffVelocity = approximateDifferential(fetchKinectPosition.OutputPosition, kinectPosition);
                    kinectVx = diffVelocity.x;
                    kinectVy = diffVelocity.y;
                    kinectVz = diffVelocity.z;
                    if (measureAccelerationOnKinect)
                    {
                        diffAcceleration = approximateDifferential( diffVelocity, formerDiffVelocity);
                        formerDiffVelocity = diffVelocity;
                    }
                }

                kinectPosition = fetchKinectPosition.OutputPosition;
            }
        }


        Matrix<float> Z = DenseMatrix.OfArray(new float[9, 1]
        {
            {imuFactor * accumulatedImuPosition.x + kinectPosition.x * kinectFactor},
            {imuFactor * accumulatedImuPosition.y + kinectFactor * kinectPosition.y},
            {imuFactor * accumulatedImuPosition.z + kinectFactor * kinectPosition.z},
            {imuFactor * accumulatedVelocity.x + kinectFactor * kinectVx},
            {imuFactor * accumulatedVelocity.y + kinectFactor * kinectVy},
            {imuFactor * accumulatedVelocity.z + kinectFactor * kinectVz},
            {ax * imuFactor + diffAcceleration.x * kinectFactor},
            {ay * imuFactor + diffAcceleration.y * kinectFactor},
            {az * imuFactor + diffAcceleration.z * kinectFactor}
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

    Vector3 accelerometerToAngles(Vector3 accData)
    {
        double phi = -Math.Atan2(accData.y, Math.Sqrt(Math.Pow(accData.x, 2) + Math.Pow(accData.z, 2))) * Mathf.Rad2Deg;
        double theta = Math.Atan2(accData.x, Math.Sqrt(Math.Pow(accData.y, 2) + Math.Pow(accData.z, 2))) *
                       Mathf.Rad2Deg;

        return new Vector3((float) phi, (float) theta, 0);
    }

    public void ResetFilter(KalmanFilter kalmanFilter)
    {
        initKalman(kalmanFilter);
    }
}

public struct KalmanFilter
{
    public Matrix<float> F;
    public Matrix<float> a;
    public Matrix<float> H;
    public Matrix<float> P;
    public Matrix<float> R;
    public Matrix<float> Q;
    public Matrix<float> x;
    public Matrix<float> K;
    public double L;
    public int N;
}