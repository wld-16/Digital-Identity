using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using UnityEngine;
using Vector3 = UnityEngine.Vector3;

public class OffsetAvatar : MonoBehaviour
{
    public bool moveBaryCenter = false;
    public List<DistanceEstimator> Estimators;
    public Transform offsterTransform;
    [SerializeField] private float initial_offset;

    // Start is called before the first frame update
    void Start()
    {
        initial_offset = offsterTransform.position.y - Estimators.Min(estimator => estimator.DistanceToGround);
        offsterTransform.position = new Vector3(offsterTransform .position.x, initial_offset, offsterTransform.position.z);
    }

    // Update is called once per frame
    void Update()
    {
        if (moveBaryCenter)
        {
            offsterTransform.position = new Vector3(offsterTransform.position.x, initial_offset - Estimators.Min(estimator => estimator.DistanceToGround), offsterTransform.position.z);
        }
    }
}
