using System.Collections.Generic;

namespace UnityEngine
{
    public interface IPushData
    {
        List<IPullData> getDataRecipients();
        void PushData();
        List<float> getData();
    }
}