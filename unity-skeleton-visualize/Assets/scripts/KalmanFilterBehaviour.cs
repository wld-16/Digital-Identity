using System;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.Distributions;
using UnityEngine;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Single;
using static System.Math;

public class KalmanFilterBehaviour : GeneralKalmanFilterBehaviour
{
    #region inputData
    
    [SerializeField] private FetchAccelerometer fetchAccelerometer;
    [SerializeField] private FetchKinectPosition fetchKinectPosition;
    
    #endregion
    
    #region kinectSpecificVariables
    [Header("kinect variables")]
    [SerializeField] private bool willProcessKinectData = false;
    [SerializeField] private Vector3  kinectPosition;
    [SerializeField] private bool measureVelocityOnKinect;
    [SerializeField] private bool measureAccelerationOnKinect;
    [SerializeField] private Vector3 formerDiffVelocity = Vector3.one;
    
    #endregion
    
    #region imuSpecificVariables
    [Header("Imu variables")]
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

    [Header("Output params")]
    [SerializeField] private bool disableX = false;
    [SerializeField] private bool disableY = false;
    [SerializeField] private bool disableZ = false;
    [SerializeField] private Vector3 dataScaleFactors = new Vector3(1, 1, 1);
    [SerializeField] private Vector3 afterScaleVector = Vector3.one;
    public Vector3 output;
    [SerializeField] private List<float> kalmanResultX2;
    [SerializeField] private bool normalizeOutput;

    #endregion

    #region generalKalmanData

    [Header("Kalman params")]
    public Matrix<float> text;
    
    public Vector3 measurementCoeffizient = Vector3.one;
    
    private KalmanFilter kalmanFilter;
    public KalmanFilter KalmanFilter => kalmanFilter;

    // Used for sensor fusion
    [SerializeField] [Range(0, 1)] private float kinectFactor = 0.5f;
    private float formerKinectFactor;
    [SerializeField] [Range(0, 1)] private float imuFactor = 0.5f;
    private float formerImuFactor;

    [SerializeField] private Vector3 kinectDebug;
    [SerializeField] private Vector3 imuDebug;
    
    [SerializeField] private List<float> kalmanPDiagonal;

    // Used for blending on reset 
    private KalmanFilter kalmanFilter2;
    
    #endregion

    #region deprecated
    [Header("Deprecated")]
    [SerializeField] private float phi_hat;
    [SerializeField] private float theta_hat;

    #endregion

    #region blendVariables

    [Header("Blending")]
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
        

        if(willProcessKinectData && !willProcessAccelerometerData){
            kalmanFilter.P = DenseMatrix.CreateDiagonal(9, 9, (i) => new float[9] {1f, 0, 0f, 1f, 0, 0f, 1f, 0, 0f}[i]);
            kalmanFilter.H = DenseMatrix.OfArray(new float[9, 9]
            {
                {0.001f, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0.001f, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0.001f, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0}
            });   
        }
        else if (!willProcessKinectData && willProcessAccelerometerData)
        {
            kalmanFilter.P = DenseMatrix.CreateDiagonal(9, 9, (i) => new float[9] {0.01f, 0, 1f, 0.01f, 0, 1f, 0.01f, 0, 1f}[i]);
            kalmanFilter.H = DenseMatrix.OfArray(new float[9, 9]
            {
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {10000, 0, 100, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 10000, 0, 100, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 10000, 0, 100000}
            });
        } else if (willProcessAccelerometerData && willProcessKinectData)
        {
            /*
            kalmanFilter.P = DenseMatrix.CreateDiagonal(9, 9, (i) => new float[9] {1000f, 0f,  1f, 1000f, 0, 1f, 1000f, 0, 1f}[i]);
            kalmanFilter.H = DenseMatrix.OfArray(new float[9, 9]
            {
                {0.01f, 0, 0f, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0.01f, 0, 0f, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0.01f, 0, 0f},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {1, 0, 10000, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 1, 0, 10000, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 1, 0, 100000}
            });
            */
            kalmanFilter.P = DenseMatrix.CreateDiagonal(9, 9, (i) => new float[9] {0.7f, 0, 0.01f, 0.7f, 0, 0.01f, 0.7f, 0, 0.01f}[i]);
            kalmanFilter.H = DenseMatrix.OfArray(new float[9, 9]
            {
                {0.0001f * 5, 0, 0f, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0.0001f * 5, 0, 0f, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0.0001f * 5, 0, 0f},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {100000/5, 0, 100, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 100000/5, 0, 100, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 10000/5, 0, 100000}
            });
        }

        mean = 0;
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
        Vector3 diffAcceleration = Vector3.zero;

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

        Matrix<float> Z = DenseMatrix.OfArray(new float[9, 1]{{0},{0},{0},{0},{0},{0},{0},{0},{0}});

        Matrix<float> kinectMatrix = null;
        Matrix<float> imuMatrix = null;
        
        if (willProcessKinectData && willProcessAccelerometerData)
        {
            if (fetchKinectPosition.OutputPosition.magnitude > 0.001f && fetchAccelerometer.rawOutput.magnitude > 0.01f)
            {
                Z = DenseMatrix.OfArray(new float[9, 1]
                {
                    {imuFactor * accumulatedImuPosition.x + kinectPosition.x * kinectFactor},
                    {imuFactor * accumulatedImuPosition.y + kinectPosition.y * kinectFactor},
                    {imuFactor * accumulatedImuPosition.z + kinectPosition.z * kinectFactor},
                    {imuFactor * accumulatedVelocity.x + kinectVx * kinectFactor},
                    {imuFactor * accumulatedVelocity.y + kinectVy * kinectFactor},
                    {imuFactor * accumulatedVelocity.z + kinectVz * kinectFactor},
                    {imuFactor * ax + diffAcceleration.x * kinectFactor},
                    {imuFactor * ay + diffAcceleration.y * kinectFactor},
                    {imuFactor * az + diffAcceleration.z * kinectFactor}
                });   
                
                kinectMatrix = DenseMatrix.OfArray(new float[9, 1]
                {
                    {kinectPosition.x * kinectFactor},
                    {kinectPosition.y * kinectFactor},
                    {kinectPosition.z * kinectFactor},
                    {kinectVx * kinectFactor},
                    {kinectVy * kinectFactor},
                    {kinectVz * kinectFactor},
                    {diffAcceleration.x * kinectFactor},
                    {diffAcceleration.y * kinectFactor},
                    {diffAcceleration.z * kinectFactor}
                });
                
                imuMatrix = DenseMatrix.OfArray(new float[9, 1]
                {
                    {imuFactor * accumulatedImuPosition.x},
                    {imuFactor * accumulatedImuPosition.y},
                    {imuFactor * accumulatedImuPosition.z},
                    {imuFactor * accumulatedVelocity.x},
                    {imuFactor * accumulatedVelocity.y},
                    {imuFactor * accumulatedVelocity.z},
                    {imuFactor * ax},
                    {imuFactor * ay},
                    {imuFactor * az}
                });   
            }
        }
        else
        {
            Z = DenseMatrix.OfArray(new float[9, 1]
            {
                {imuFactor * accumulatedImuPosition.x + kinectPosition.x * kinectFactor},
                {imuFactor * accumulatedImuPosition.y + kinectPosition.y * kinectFactor},
                {imuFactor * accumulatedImuPosition.z + kinectPosition.z * kinectFactor},
                {imuFactor * accumulatedVelocity.x + kinectVx * kinectFactor},
                {imuFactor * accumulatedVelocity.y + kinectVy * kinectFactor},
                {imuFactor * accumulatedVelocity.z + kinectVz * kinectFactor},
                {imuFactor * ax + diffAcceleration.x * kinectFactor},
                {imuFactor * ay + diffAcceleration.y * kinectFactor},
                {imuFactor * az + diffAcceleration.z * kinectFactor}
            });   
        }


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
            kalmanFilter = KalmanUpdate(Z, kalmanFilter, kinectMatrix, imuMatrix);

            Dictionary<string, Matrix<float>> kalmanPredict = KalmanPredict(kalmanFilter);

            //transform.position = new Vector3(kalmanPredict["x"].At(0,0) * N,kalmanPredict["x"].At(1,0) * N,kalmanPredict["x"].At(2,0) * N);
            kalmanResultX = kalmanPredict["x"].ToRowMajorArray().ToList();


            kalmanPDiagonal = kalmanPredict["P"].Diagonal().ToList();


            Vector3 prediction = ((new Vector3(
                disableX ? 0 : kalmanResultX[observationOutputMapping[0]],
                disableY ? 0 : kalmanResultX[observationOutputMapping[1]],
                disableZ ? 0 : kalmanResultX[observationOutputMapping[2]])));

            if (useBlendByLikelyHood)
            {
                Debug.Log("Likelyhood: " + kalmanFilter.L);   
            }

            prediction.Scale(afterScaleVector);
            
            outputData = new List<float>(){prediction.x,prediction.y,prediction.z};
            output = prediction;
        }
    }


    private KalmanFilter KalmanUpdate(Matrix<float> z, KalmanFilter kalmanFilter, Matrix<float> debugKinectMatrix = null, Matrix<float> debugImuMatrix = null)
    {
        if (debugImuMatrix != null)
        {
            Matrix<float> Si = kalmanFilter.H * kalmanFilter.P * kalmanFilter.H.Transpose() + kalmanFilter.R;
            Matrix<float> Ki = kalmanFilter.P * kalmanFilter.H.Transpose() * Si.Inverse();

            Matrix<float> yi = debugImuMatrix - kalmanFilter.H * kalmanFilter.x;
            Matrix<float> xi = kalmanFilter.x + Ki * yi;

            
            Matrix<float> imuX = kalmanFilter.F * xi;
            List<float> floats = imuX.ToRowMajorArray().ToList();
            
            imuDebug = new Vector3(floats[0],floats[1],0);
            
            Debug.DrawRay(transform.position, (imuDebug-transform.localPosition).normalized * 2, Color.red);
        }

        if (debugKinectMatrix != null)
        {
            Matrix<float> Sk = kalmanFilter.H * kalmanFilter.P * kalmanFilter.H.Transpose() + kalmanFilter.R;
            Matrix<float> Kk = kalmanFilter.P * kalmanFilter.H.Transpose() * Sk .Inverse();

            Matrix<float> yk = debugKinectMatrix - kalmanFilter.H * kalmanFilter.x;
            Matrix<float> xk = kalmanFilter.x + Kk * yk;

            Matrix<float> kinectX = kalmanFilter.F * xk;
            List<float> floats = kinectX.ToRowMajorArray().ToList();

            kinectDebug = new Vector3(floats[0],floats[1],0);
            Debug.DrawRay(transform.position, (kinectDebug-transform.localPosition).normalized * 2,Color.blue);
        }

        Matrix<float> S = kalmanFilter.H * kalmanFilter.P * kalmanFilter.H.Transpose() + kalmanFilter.R;
        Matrix<float> K = kalmanFilter.P * kalmanFilter.H.Transpose() * S.Inverse();

        Matrix<float> y = z - kalmanFilter.H * kalmanFilter.x;
        Matrix<float> x = kalmanFilter.x + K * y;
        Matrix<float> P = (DenseMatrix.CreateIdentity(9) - K * kalmanFilter.H) * kalmanFilter.P;

        if (debugImuMatrix != null && debugKinectMatrix != null)
        {
            Matrix<float> prediction = kalmanFilter.F * x;
            List<float> floats = prediction.ToRowMajorArray().ToList();
            
            Debug.DrawRay(transform.position, (new Vector3(floats[0],floats[1],0)-transform.localPosition).normalized,Color.green);
        }

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
    
    private void OnValidate()
    {
       
    }
    
    public new List<float> getData()
    {
        return outputData;
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