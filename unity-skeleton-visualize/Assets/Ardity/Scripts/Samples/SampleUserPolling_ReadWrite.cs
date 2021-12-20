/**
 * Ardity (Serial Communication for Arduino + Unity)
 * Author: Daniel Wilches <dwilches@gmail.com>
 *
 * This work is released under the Creative Commons Attributions license.
 * https://creativecommons.org/licenses/by/2.0/
 */

using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Data;

/**
 * Sample for reading using polling by yourself, and writing too.
 */
public class SampleUserPolling_ReadWrite : MonoBehaviour
{
    public SerialController serialController;
    public Vector3 ringOrientation = Vector3.zero;
    
    public Orientations orientations;

    [SerializeField] private List<AccelerationMapping> AccelerationMapping = new List<AccelerationMapping>();    

    // Initialization
    void Start()
    {
        serialController = GameObject.Find("SerialController").GetComponent<SerialController>();

        Debug.Log("Press A or Z to execute some actions");
    }

    // Executed each frame
    void FixedUpdate()
    {
        //---------------------------------------------------------------------
        // Send data
        //---------------------------------------------------------------------

        // If you press one of these keys send it to the serial device. A
        // sample serial device that accepts this input is given in the README.
        if (Input.GetKeyDown(KeyCode.J))
        {
            Debug.Log("Sending J");
            serialController.SendSerialMessage("j");
        }

        if (Input.GetKeyDown(KeyCode.S))
        {
            Debug.Log("Sending S");
            serialController.SendSerialMessage("s");
        }
        
        if (Input.GetKeyDown(KeyCode.R))
        {
            Debug.Log("Sending R");
            serialController.SendSerialMessage("r");
        }
        
        if (Input.GetKeyDown(KeyCode.F))
        {
            Debug.Log("Sending F");
            serialController.SendSerialMessage("f");
        }
        
        if (Input.GetKeyDown(KeyCode.Return))
        {
            Debug.Log("Sending Return");
            serialController.SendSerialMessage("Return");
        }


        //---------------------------------------------------------------------
        // Receive data
        //---------------------------------------------------------------------

        string message = serialController.ReadSerialMessage();

        if (message == null)
            return;

        // Check if the message is plain data or a connect/disconnect event.
        if (ReferenceEquals(message, SerialController.SERIAL_DEVICE_CONNECTED))
            Debug.Log("Connection established");
        else if (ReferenceEquals(message, SerialController.SERIAL_DEVICE_DISCONNECTED))
            Debug.Log("Connection attempt failed or disconnection detected");
        else
        {
            //Debug.Log("Message arrived: " + message);
            try
            {
                orientations = JsonUtility.FromJson<Orientations>(message);
                Debug.Log(orientations);
            }
            catch (ArgumentException aex)
            {
                //Debug.LogWarning("Caugth json parse exception: " + aex.Message);
            }
            
        }
        
    }
}


