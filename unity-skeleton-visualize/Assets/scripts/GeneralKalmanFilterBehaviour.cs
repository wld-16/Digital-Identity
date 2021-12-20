using System;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.Distributions;
using UnityEngine;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Single;
using UnityEditor;
using static System.Math;
using Matrix4x4 = System.Numerics.Matrix4x4;
using Object = System.Object;

public class GeneralKalmanFilterBehaviour : MonoBehaviour, IPushData, IPullData
{
    #region api

    [SerializeField] private List<Component> inputComponent;
    [SerializeField] private List<Component> outputComponents;

    #endregion

    #region inputData

    [Header("Input data")] [SerializeField]
    private List<float> inputData;

    #endregion

    #region outData

    [Header("Output data")] [SerializeField]
    protected Vector3 dataOffset = new Vector3(0, 0, 0);

    [SerializeField] protected int[] observationOutputMapping = new int[3];
    [SerializeField] protected List<float> kalmanResultX;
    public List<float> outputData;

    #endregion

    #region generalKalmanData

    [Header("Kalman params")] [SerializeField]
    protected FloatDictionary sigmaValues;

    [SerializeField] protected List<float> x;
    [SerializeField] protected FloatMatrix P;
    [SerializeField] protected FloatMatrix F;
    [SerializeField] protected FloatMatrix H;
    [SerializeField] protected float dt = 14.0f / 1000.0f;
    [SerializeField] protected double mean = 0;
    [SerializeField] protected double stdDev = 2.56;
    private KalmanFilter kalmanFilter;
    public KalmanFilter KalmanFilter => kalmanFilter;

    #endregion

    #region options

    [SerializeField] private bool useDynamicOrientationTransition;
    [SerializeField] private bool useDynamicAccelerationTransition;

    #endregion

    public void Awake()
    {
        if (typeof(IPullData) != inputComponent.GetType().GetInterface("IPullData"))
        {
            Debug.LogWarning("InputComponent on " + name + " needs to implement the IPullData Interface!!");
        }

        if (!outputComponents.Select(component => typeof(IPushData) == component.GetType().GetInterface("IPushData"))
            .Aggregate((b, b1) => b && b1))
        {
            Debug.LogWarning("Every Component of OutputComponents on " + name +
                             " needs to implement the IPullData Interface!!");
        }
    }

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


        if (useDynamicAccelerationTransition)
        {
            List<float> data = getDataDeliverer(typeof(FetchTransformationMatrixOfKinect)).getData();
            Matrix<float> dataMatrix = DenseMatrix.OfArray(new float[3, 3]
            {
                {data[0], data[3], data[6]},
                {data[1], data[4], data[7]},
                {data[2], data[5], data[8]}
            });
            kalmanFilter.H = UpdateDynamicFWithTransformation(dataMatrix);
        }

        if (useDynamicOrientationTransition)
        {
            List<float> data = getDataDeliverer(typeof(FetchOrientationOfKinect)).getData();
            kalmanFilter.H = UpdateDynamicFWithQuaternion(new Quaternion(data[1], data[2], data[3], data[0]));
        }


        kalmanFilter = KalmanUpdate(Z, kalmanFilter);

        Dictionary<string, Matrix<float>> kalmanPredict = KalmanPredict(kalmanFilter);

        kalmanResultX = kalmanPredict["x"].ToRowMajorArray().ToList();

        outputData = observationOutputMapping.Select(index => kalmanResultX[index]).ToList();
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
    }

    Matrix<float> UpdateDynamicFWithQuaternion(Quaternion quat)
    {
        return DenseMatrix.OfArray(new float[7, 7]
        {
            {1f, 0f, 0f, 0f, -quat.x * dt / 2, -quat.y, -quat.z * dt / 2},
            {0, 1f, 0, 0, quat.w * dt / 2, quat.z, quat.y * dt / 2},
            {0, 0, 1, 0, quat.z * dt / 2, quat.w * dt / 2, -quat.x * dt / 2},
            {0, 0, 0, 1, -quat.y * dt / 2, quat.x * dt / 2, quat.w * dt / 2},
            {0, 0, 0, 0, 1, 0, 0},
            {0, 0, 0, 0, 0, 1, 0},
            {0, 0, 0, 0, 0, 0, 1}
        });
    }

    Matrix<float> UpdateDynamicFWithTransformation(Matrix<float> handToLocal)
    {
        return DenseMatrix.OfArray(new float[9, 9]
        {
            {
                1f, dt, (float) (handToLocal[0, 0] * Pow(dt, 2) / 2), 0, 0,
                (float) (handToLocal[1, 0] * Pow(dt, 2) / 2), 0, 0, (float) (handToLocal[2, 0] * Pow(dt, 2) / 2)
            },
            {0, 1, handToLocal[0, 0] * dt, 0, 0, handToLocal[1, 0] * dt, 0, 0, handToLocal[2, 0] * dt},
            {0, 0, 1, 0, 0, 0, 0, 0, 0},
            {
                0, dt, (float) (handToLocal[0, 1] * Pow(dt, 2) / 2), 1, dt,
                (float) (handToLocal[1, 1] * Pow(dt, 2) / 2), 0, 0, (float) (handToLocal[2, 2] * Pow(dt, 2) / 2)
            },
            {0, 0, handToLocal[0, 1] * dt, 0, 1, handToLocal[1, 1] * dt, 0, 0, handToLocal[2, 1] * dt},
            {0, 0, 0, 0, 0, 1, 0, 0, 0},
            {
                0, dt, (float) (handToLocal[0, 2] * Pow(dt, 2) / 2), 0, 0, (float) (handToLocal[1, 2] * Pow(dt, 2) / 2),
                1, dt, (float) (handToLocal[2, 2] * Pow(dt, 2) / 2)
            },
            {0, 0, handToLocal[0, 2] * dt, 0, 0, handToLocal[1, 2] * dt, 0, 1, handToLocal[2, 2] * dt},
            {0, 0, 0, 0, 0, 0, 0, 0, 1}
        });
    }

    public List<IPullData> getDataRecipients()
    {
        return outputComponents.Select(component => (IPullData) component).ToList();
    }

    public void PushData()
    {
        getDataRecipients().ForEach(recipient => recipient.Receive(outputData));
    }

    public List<float> getData()
    {
        return outputData;
    }

    public IPushData getDataDeliverer(Type type)
    {
        return (IPushData) inputComponent.Find(component => component.GetType() == type);
    }

    void IPullData.PullData(Type type)
    {
        inputData = getDataDeliverer(type).getData();
    }

    public void Receive(List<float> receivedData)
    {
        inputData = receivedData;
    }
}