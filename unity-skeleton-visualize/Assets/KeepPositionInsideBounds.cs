using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class KeepPositionInsideBounds : MonoBehaviour
{

    [SerializeField] private Bounds bounds;
    [SerializeField] private bool centerIsPosition = true;
    [SerializeField] private Rigidbody rb;
    [SerializeField] private Transform positionTransformToCheck;

    // Start is called before the first frame update
    void Start()
    {
        if(centerIsPosition)
        {
            bounds.center = transform.position;
        }
    }

    // Update is called once per frame
    void Update()
    {
        if (!bounds.Contains(positionTransformToCheck.position))
        {
            transform.position = bounds.ClosestPoint(positionTransformToCheck.position); 
            rb.velocity = Vector3.zero;
            //rb.AddForce(-(positionTransformToCheck.position - bounds.ClosestPoint(positionTransformToCheck.position)));
        }
    }
}
