#ifndef ImuUnit_H
#define ImuUnit_H

#include "MPU6050_Wrapper.h"
#include <ArduinoJson.h>

class ImuUnit {
  public:
    ImuUnit(){}

    ImuUnit(uint8_t mpuId):_mpuId(mpuId) {

    }

    Quaternion getQuaternion(){
      return q;
    }

    VectorInt16 getAcceleration(){
      return accWorld;
    }

    VectorFloat getGravity() {
      return gravity;
    }

    float* getYPR(){
      return ypr;
    }

    float getYaw(){
      return ypr[0]  * 180 / M_PI;
    }

    float getPitch(){
      return ypr[1]  * 180 / M_PI;
    }

    float getRoll(){
      return ypr[2]  * 180 / M_PI;
    }

    void setMpuId(uint8_t mpuId){
      _mpuId = mpuId;
    }

    void setIsPrintingJson(bool isPrintingJson){
      _isPrintingJson = isPrintingJson;
    }

    uint8_t getMpuId(){
      return _mpuId;
    }

    void setOffset(MPU6050_Wrapper* currentMPU, int x_offset_gyro, int y_offset_gyro, int z_offset_gyro,
                                         int x_offset_acc, int y_offset_acc, int z_offset_acc){
      // supply your own gyro offsets here, scaled for min sensitivity
    currentMPU->_mpu.setXGyroOffset(x_offset_gyro);
    currentMPU->_mpu.setYGyroOffset(y_offset_gyro);
    currentMPU->_mpu.setZGyroOffset(z_offset_gyro);
    currentMPU->_mpu.setXAccelOffset(x_offset_acc);
    currentMPU->_mpu.setYAccelOffset(y_offset_acc);
    currentMPU->_mpu.setZAccelOffset(z_offset_acc);
    
    }

    void handleMPUevent(MPU6050_Wrapper* currentMPU, uint8_t mpu){
      // reset interrupt flag and get INT_STATUS byte
      currentMPU->getIntStatus();

      // check for overflow (this should never happen unless our code is too inefficient)
      if ((currentMPU->_mpuIntStatus & _BV(MPU6050_INTERRUPT_FIFO_OFLOW_BIT))
          || currentMPU->_fifoCount >= 1024) {
        // reset so we can continue cleanly
        currentMPU->resetFIFO();
        return;
      }
      // otherwise, check for DMP data ready interrupt (this should happen frequently)
      if (currentMPU->_mpuIntStatus & _BV(MPU6050_INTERRUPT_DMP_INT_BIT)) {

        // read and dump a packet if the queue contains more than one
        while (currentMPU->_fifoCount >= 2 * currentMPU->_packetSize) {
          // read and dump one sample
          //Serial.print("DUMP"); // this trace will be removed soon
          currentMPU->getFIFOBytes(fifoBuffer);
        }

        // read a packet from FIFO
        currentMPU->getFIFOBytes(fifoBuffer);
        
        currentMPU->_mpu.dmpGetQuaternion(&q, fifoBuffer);
        currentMPU->_mpu.dmpGetGravity(&gravity, &q);
        currentMPU->_mpu.dmpGetAccel(&accRaw, fifoBuffer);
        currentMPU->_mpu.dmpGetLinearAccel(&aaReal, &accRaw, &gravity);
        currentMPU->_mpu.dmpGetLinearAccelInWorld(&accWorld, &aaReal, &q);
        currentMPU->_mpu.dmpGetYawPitchRoll(ypr, &q, &gravity);
        
      }
    }

    float ypr[3];        // [yaw, pitch, roll]   yaw/pitch/roll container and gravity vector
    VectorInt16 acc;
    VectorInt16 aaReal;
    VectorInt16 accRaw;
    VectorInt16 accWorld;
    uint8_t fifoBuffer[64]; // FIFO storage buffer
    VectorFloat gravity; // [x, y, z]            gravity vector
    Quaternion q;        // [w, x, y, z]         quaternion container
    uint8_t _mpuId;
    bool _isPrintingJson = false;
    
};

#endif // IMUUNIT_H
