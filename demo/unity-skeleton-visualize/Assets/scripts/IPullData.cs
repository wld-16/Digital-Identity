using System;
using System.Collections.Generic;

namespace UnityEngine
{
    public interface IPullData
    {
        IPushData getDataDeliverer(Type type);
        void PullData(Type type);
        void Receive(List<float> receivedData);
    }

    enum PULL_TYPE
    {
        KALMAN_FILTER,
        ACCELERATION
    }
}