using System.Collections.Generic;

namespace UnityEngine
{
    public interface IPullData
    {
        IPushData getDataDeliverer();
        void PullData();
        void Receive(List<float> receivedData);
    }
}