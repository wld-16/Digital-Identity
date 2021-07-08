// I2Cdev and MPU6050 müssen als Libraries installiert sein
#include "src/I2Cdev.h"
#include "src/PixelRingDisplay.h"
#include "src/MPU6050_Wrapper.h"
#include "src/TogglePin.h"
#include "src/DeathTimer.h"
#include "src/ImuUnit.h"
#include "Wire.h"


MPU6050_Array mpus(2);

#define AD0_PIN_0 4  // Ring
#define AD0_PIN_1 5  // Handgelenk

#define LED_PIN 13

TogglePin activityLed(LED_PIN, 100);
DeathTimer deathTimer(5000L);
PixelRingDisplay pixelRingDisplay;
ImuUnit imuUnits[2];

/*
struct StringFloatPair{
  String key;
  float value;
};

struct StringFloatPair *ringData;
struct StringFloatPair *fingerData;
*/

bool isMeasuringWithTwoSensors = false;
bool isPrintingJson = true;
bool isOutputingRawSensorData = true;

// --- SETUP --- //

void setup() {
  pixelRingDisplay.initStrip();
  
  // join I2C bus (I2Cdev library doesn't do this automatically)
  #if I2CDEV_IMPLEMENTATION == I2CDEV_ARDUINO_WIRE
    Wire.begin();
    Wire.setClock(400000); // 400kHz I2C clock. Comment this line if having compilation difficulties
  #elif I2CDEV_IMPLEMENTATION == I2CDEV_BUILTIN_FASTWIRE
    Fastwire::setup(400, true);
  #endif

  Serial.begin(115200);

  mpus.initialize();

  // configure LED for output
  pinMode(LED_PIN, OUTPUT);

  // verify connection
  Serial.println(F("Testing device connections..."));
  if (mpus.testConnection()) {
    Serial.println(F("MPU6050 connection successful"));
  } else {
    mpus.halt(F("MPU6050 connection failed, halting"));
  }

  // wait for ready
  Serial.println(F("\nPress [f] to record finger movement"));
  Serial.println(F("Press [j] for json output"));
  Serial.println(F("Press [r] to receive acceleration and gyro output"));
  Serial.println(F("Press [s] to begin DMP programming and demo: "));
  while (true){
    pixelRingDisplay.displayLoading();
    
    if(Serial.available()){
      int input = Serial.read();
      if(input == 102){                               // 102 is 'f' as char
        isMeasuringWithTwoSensors = !isMeasuringWithTwoSensors;
        Serial.print(F("Does measure Finger sensor: "));
        Serial.println(isMeasuringWithTwoSensors);
      } else if(input == 106){                        // 106 is 'j' as char
        isPrintingJson = !isPrintingJson;
        Serial.print(F("Is printing Json Data: "));
        Serial.println(isPrintingJson);
      } 
      else if(input == 115){                          // 13 is '\n' as char
        Serial.println("Pressed s");
        break;
      } else if(input == 114){
        isOutputingRawSensorData= !isOutputingRawSensorData;
        Serial.print("Will Output Raw Sensor Data: ");
        Serial.println(isOutputingRawSensorData);
      } 
      else {
        Serial.println(input);
      }
    }
  }

  // initialize device
  Serial.println(F("Initializing I2C devices..."));
  mpus.add(AD0_PIN_1);

  imuUnits[0].setMpuId(0);
  imuUnits[0].setIsPrintingJson(isPrintingJson);
  
  if(isMeasuringWithTwoSensors){
    imuUnits[1].setMpuId(1); 
    imuUnits[1].setIsPrintingJson(isPrintingJson);
    mpus.add(AD0_PIN_0);
  }
  
  activityLed.setPeriod(500); // slow down led to 2Hz

  // load and configure the DMP
  Serial.println(F("Initializing DMP..."));
  mpus.dmpInitialize();

  // supply your own gyro offsets here, scaled for min sensitivity
  imuUnits[0].setOffset(mpus.select(imuUnits[0].getMpuId()),124,79,-10,-5470,-826,1016);
  if(isMeasuringWithTwoSensors){
    imuUnits[1].setOffset(mpus.select(imuUnits[1].getMpuId()),55,3,12,-3045,-1644,752); 
  }
  mpus.programDmp(0);
  if(isMeasuringWithTwoSensors){
    mpus.programDmp(1); 
  }
  Serial.println(F("done"));
  if(!isPrintingJson){
    if(!isOutputingRawSensorData){
        Serial.print("ring.yaw,\tring.pitch,\tring.roll,"); 
      if(isMeasuringWithTwoSensors){
        Serial.print("hand.yaw,\thand.pitch,\thand.roll,"); 
      }
      Serial.println("");
    } else {
      Serial.print("ringAcc.x,\tringAcc.y,\tringAcc.z,\tringGyro.x,\tringGyro.y,\tringGyro.z,\tringGyro.w,");
      if(isMeasuringWithTwoSensors){
        Serial.print("handAcc.x,\thandAcc.y,\thandAcc.z,\thandGyro.x,\thandGyro.y,\thandGyro.z,\thandGyro.w,");  
      }
      Serial.println("");
    }
  }
}

// --- Hauptprogramm --- //

void loop() {
 
  static uint8_t mpu = 0;
  static MPU6050_Wrapper* currentMPU = NULL;

  
  for (int i=0; i<  (isMeasuringWithTwoSensors   ? 2  : 1) ; i++) {
    mpu=(mpu+1)%2; // failed attempt at round robin
    currentMPU = mpus.select(mpu);
    if (currentMPU->isDue()) {
       imuUnits[mpu].handleMPUevent(mpus.select(mpu), mpu, isOutputingRawSensorData);
    }
  }

  if(isPrintingJson){
    StaticJsonDocument<200> doc;
    if(!isOutputingRawSensorData){
      doc["ringAxis"]["yaw"] = imuUnits[0].getYaw();
      doc["ringAxis"]["pitch"] = imuUnits[0].getPitch();
      doc["ringAxis"]["roll"] = imuUnits[0].getRoll();
      if(isMeasuringWithTwoSensors){
        doc["handAxis"]["yaw"] = imuUnits[1].getYaw();
        doc["handAxis"]["pitch"] = imuUnits[1].getPitch();
        doc["handAxis"]["roll"] = imuUnits[1].getRoll();
        }  
    } else {
      Quaternion quat = imuUnits[0].getQuaternion();
      VectorInt16 acc = imuUnits[0].getAcceleration();
      doc["ring"]["gyroscope"]["x"] = quat.x;
      doc["ring"]["gyroscope"]["y"] = quat.y;
      doc["ring"]["gyroscope"]["z"] = quat.z;
      doc["ring"]["gyroscope"]["w"] = quat.w;
      doc["ring"]["accelerometer"]["x"] = acc.x;
      doc["ring"]["accelerometer"]["y"] = acc.y;
      doc["ring"]["accelerometer"]["z"] = acc.z;
      if(isMeasuringWithTwoSensors){
        quat = imuUnits[1].getQuaternion();
        acc = imuUnits[1].getAcceleration();
        doc["hand"]["gyroscope"]["x"] = quat.x;
        doc["hand"]["gyroscope"]["y"] = quat.y;
        doc["hand"]["gyroscope"]["z"] = quat.z;
        doc["hand"]["gyroscope"]["w"] = quat.w;
        doc["hand"]["accelerometer"]["x"] = acc.x;
        doc["hand"]["accelerometer"]["y"] = acc.y;
        doc["hand"]["accelerometer"]["z"] = acc.z;
        }  
    }
    
    serializeJson(doc, Serial);
  } else {
    if(!isOutputingRawSensorData){
        Serial.print("" + String(imuUnits[0].getPitch()) + ",\t" + String(imuUnits[0].getPitch())+ ",\t" + String(imuUnits[0].getRoll()) + ",\t"); 
        if(isMeasuringWithTwoSensors){
          Serial.print("" + String(imuUnits[1].getYaw()) + ",\t"+ String(imuUnits[1].getPitch()) + ",\t" + String(imuUnits[1].getRoll()) + ","); 
        }
    } else {
      Quaternion quat = imuUnits[0].getQuaternion();
      VectorInt16 acc = imuUnits[0].getAcceleration();
     
      char cstr[60];
      char floatStrX[8]; // 
      char floatStrY[8];
      char floatStrZ[8];
      char floatStrW[8];
      dtostrf(quat.x, 6, 4, floatStrX);
      dtostrf(quat.y, 6, 4, floatStrY);
      dtostrf(quat.z, 6, 4, floatStrZ);
      dtostrf(quat.w, 6, 4, floatStrW);
      sprintf(cstr, "%d,%d,%d,%s,%s,%s,%s,", acc.x, acc.y, acc.z, floatStrX, floatStrY, floatStrZ, floatStrW);
      Serial.print(cstr);
      if(isMeasuringWithTwoSensors){
        quat = imuUnits[1].getQuaternion();
        acc = imuUnits[1].getAcceleration();
        Serial.print("" + String(acc.x) + "," + String(acc.y) + "," + String(acc.z) + "," + String(quat.x) + "," + String(quat.y) + "," + String(quat.z) + "," + String(quat.w) + ",");
      }
    }
  }

  pixelRingDisplay.update(imuUnits[1].getPitch(), imuUnits[0].getPitch(), imuUnits[1].getRoll(), imuUnits[0].getRoll(), imuUnits[1].getYaw());

  
  Serial.println("");
  activityLed.update();
  deathTimer.update();
}
