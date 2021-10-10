using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using static UnityEngine.Mathf;

public class DistanceEstimator : MonoBehaviour
{
    [SerializeField] private Vector3 direction;
    [SerializeField] private bool downwards;
    private Ray _ray;
    [Range(0, 5f)] [SerializeField] private float rayDistance = 0.5f;  
    [SerializeField] private float distanceToGround;

    public float DistanceToGround => distanceToGround;

    // Start is called before the first frame update
    void Start()
    {
        direction = downwards ? Vector3.down : direction;
        _ray = new Ray(transform.position, direction);
        RaycastHit hit;
        if (Physics.Raycast(_ray, out hit, rayDistance))
        {
            distanceToGround = hit.distance;
            Debug.Log(name + " distance to ground " + hit.distance);
            Debug.DrawRay(transform.position,direction, Color.red);
        };
    }

    // Update is called once per frame
    void Update()
    {
        _ray.origin = transform.position;
        RaycastHit hit;
        if (Physics.Raycast(_ray, out hit, rayDistance))
        {
            distanceToGround = hit.distance;
            Debug.Log(name + " distance to ground " + hit.distance);
            Debug.DrawRay(transform.position,direction, Color.red);
        };
    }
}
