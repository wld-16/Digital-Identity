// I2Cdev and MPU6050 müssen als Libraries installiert sein
#include "I2Cdev.h"
#include "PixelRingDisplay.h"
#include "MPU6050_Wrapper.h"
#include "TogglePin.h"
#include "DeathTimer.h"
#include "ImuUnit.h"
#include "Wire.h"


MPU6050_Array mpus(2);

#define AD0_PIN_0 4  // Ring
#define AD0_PIN_1 5  // Handgelenk

#define LED_PIN 13

TogglePin activityLed(LED_PIN, 100);
DeathTimer deathTimer(5000L);
PixelRingDisplay pixelRingDisplay;
ImuUnit imuUnit;

/*
struct StringFloatPair{
  String key;
  float value;
};

struct StringFloatPair *ringData;
struct StringFloatPair *fingerData;
*/

bool isPrintingJson = true;
bool isOutputtingGyroQuaternionData = true;
bool isOutputtingGyroEulerAnglesData = true;
bool isOutputtingAccelerometerData = true;
bool isOutputtingGravityData = true;

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
  Serial.println(F("Press [j] to toggle json output"));
  Serial.println(F("Press [r] to distplay range mode"));
  Serial.println(F("Press [g] to toggle gravity data"));
  Serial.println(F("Press [e] to toggle gyroscope euler data"));
  Serial.println(F("Press [w] to toggle gyroscope quaternion data"));
  Serial.println(F("Press [a] to toggle accelerometer data"));
  Serial.println(F("Press [s] to begin DMP programming and demo: "));
  while (true){
    pixelRingDisplay.displayLoading();
    
    if(Serial.available()){
      int input = Serial.read();
      if(input == 106){                        // 106 is 'j' as char
        isPrintingJson = !isPrintingJson;
        Serial.print(F("Is printing Json Data: "));
        Serial.println(isPrintingJson);
      } 
      else if(input == 97){                          // 97 is 'a' as char
        isOutputtingAccelerometerData = !isOutputtingAccelerometerData;
        Serial.print("Will output accelerometer: ");
        Serial.println(isOutputtingAccelerometerData);
      }
      else if(input == 114){                          // 114 is 'r' as char
        Serial.print("Range Mode Accelerometer: ");
        Serial.println(mpus.select(0)->_mpu.getFullScaleAccelRange());
        Serial.print("Range Mode Gyroscope: ");
        Serial.println(mpus.select(0)->_mpu.getFullScaleGyroRange());
      }
      else if(input == 115){                          // 13 is '\n' as char
        Serial.println("Pressed s");
        break;
      } else if(input == 101){ // 101 is 'e' in ascii
        isOutputtingGyroEulerAnglesData = !isOutputtingGyroEulerAnglesData;
        Serial.print("Will output gyro euler angles data: ");
        Serial.println(isOutputtingGyroEulerAnglesData);
      } else if(input == 103) { // 103 is 'g' in ascii
        isOutputtingGravityData = !isOutputtingGravityData;
        Serial.print("Will output gravity: ");
        Serial.println(isOutputtingGravityData);
      } else if(input == 119 ) { // 119 is 'w' in ascii
        isOutputtingGyroQuaternionData = !isOutputtingGyroQuaternionData;
        Serial.print("Will output gyro quaternion data: ");
        Serial.println(isOutputtingGyroQuaternionData);
      }
      else {
        Serial.println(input);
      }
    }
  }

  // initialize device
  Serial.println(F("Initializing I2C devices..."));
  mpus.add(AD0_PIN_1);

  imuUnit.setMpuId(0);
  imuUnit.setIsPrintingJson(isPrintingJson);
  
  activityLed.setPeriod(500); // slow down led to 2Hz

  // load and configure the DMP
  Serial.println(F("Initializing DMP..."));
  mpus.dmpInitialize();

  // supply your own gyro offsets here, scaled for min sensitivity
  imuUnit.setOffset(mpus.select(imuUnit.getMpuId()),124,79,-10,-5470,-826,1016);
  mpus.programDmp(0);
  Serial.println(F("done"));
  if(!isPrintingJson){
        Serial.print("ringAcc.x,\tringAcc.y,\tringAcc.z,\tringGyro.x,\tringGyro.y,\tringGyro.z,\tringGyro.w,\tring.yaw,\tring.pitch,\tring.roll,");
        Serial.println("");
  }
}

// --- Hauptprogramm --- //

void loop() {
 
  static uint8_t mpu = 0;
  static MPU6050_Wrapper* currentMPU = NULL;

  
  for (int i=0; i<  1 ; i++) {
    mpu=(mpu+1)%2; // failed attempt at round robin
    currentMPU = mpus.select(mpu);
    if (currentMPU->isDue()) {
       imuUnit.handleMPUevent(mpus.select(mpu), mpu);
    }
  }

  if(isPrintingJson){
    StaticJsonDocument<200> doc;
      Quaternion quat = imuUnit.getQuaternion();
      VectorInt16 acc = imuUnit.getAcceleration();
      VectorFloat gravity = imuUnit.getGravity();
      
      if(isOutputtingGyroQuaternionData){
        doc["ring"]["gyroscope"]["x"] = quat.x;
        doc["ring"]["gyroscope"]["y"] = quat.y;
        doc["ring"]["gyroscope"]["z"] = quat.z;
        doc["ring"]["gyroscope"]["w"] = quat.w;  
      }
      if(isOutputtingGyroEulerAnglesData){
        doc["ring"]["gyroscope"]["yaw"] = imuUnit.getYaw();
        doc["ring"]["gyroscope"]["pitch"] = imuUnit.getPitch();
        doc["ring"]["gyroscope"]["roll"] = imuUnit.getRoll();
      }
      if(isOutputtingAccelerometerData){
        doc["ring"]["accelerometer"]["x"] = ((float) acc.x) / 8092 * 9.81;
        doc["ring"]["accelerometer"]["y"] = ((float) acc.y) / 8092 * 9.81;
        doc["ring"]["accelerometer"]["z"] = ((float) acc.z) / 8092 * 9.81;
      }
      if(isOutputtingGravityData){
        doc["ring"]["gravity"]["x"] = gravity.x;
        doc["ring"]["gravity"]["y"] = gravity.y;
        doc["ring"]["gravity"]["z"] = gravity.z; 
      }
      serializeJson(doc, Serial);
  } else {
      
      Quaternion quat = imuUnit.getQuaternion();
      VectorInt16 acc = imuUnit.getAcceleration();

      if(isOutputtingAccelerometerData) {
        Serial.print(((float) acc.x) / 8092 * 9.81);
        Serial.print(",\t\t");
        Serial.print(((float) acc.y) / 8092 * 9.81);
        Serial.print(",\t\t");
        Serial.print(((float) acc.z) / 8092 * 9.81);
        Serial.print(",\t\t");
      }

      if(isOutputtingGyroQuaternionData){
      
      char floatStrX[9]; // 
      char floatStrY[9];
      char floatStrZ[9];
      char floatStrW[9];
        
        dtostrf(quat.x, 8, 5, floatStrX);
        dtostrf(quat.y, 8, 5, floatStrY);
        dtostrf(quat.z, 8, 5, floatStrZ);
        dtostrf(quat.w, 8, 5, floatStrW);  
        
        Serial.print(floatStrX);
        Serial.print(",\t");
        Serial.print(floatStrY);
        Serial.print(",\t");
        Serial.print(floatStrZ);
        Serial.print(",\t");
        Serial.print(floatStrW);
        Serial.print(",\t");
      }

      if(isOutputtingGyroEulerAnglesData){
        char floatStrX[9]; // 
        char floatStrY[9];
        char floatStrZ[9];
        
        dtostrf(imuUnit.getYaw(), 5, 3, floatStrX);
        dtostrf(imuUnit.getPitch(), 5, 3, floatStrY);
        dtostrf(imuUnit.getRoll(), 5, 3, floatStrZ);
         
        Serial.print(floatStrX);
        Serial.print(",\t\t");
        Serial.print(floatStrY);
        Serial.print(",\t\t");
        Serial.print(floatStrZ);
        Serial.print(",\t\t");
      }
  }

  pixelRingDisplay.update(imuUnit.getPitch(), imuUnit.getPitch(), imuUnit.getRoll(), imuUnit.getRoll(), imuUnit.getYaw());
  Serial.println("");
  activityLed.update();
  deathTimer.update();
}
