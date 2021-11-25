#ifndef PixelRingDisplay_H
#define PixelRingDisplay_H

#include "Adafruit_NeoPixel.h"
#include "MPU6050_Wrapper.h"

class PixelRingDisplay {
  public:
    void update(int pitchH, int pitchR, int rollH, int rollR, int yawH){
      int pitchDiff = abs(pitchR - pitchH);
      int rollAbs = (rollR + rollH) / 2;
      if (rollAbs < -45){
        rollAbs = -45;
      }else if (rollAbs > 45){
        rollAbs = 45;
      }

      fadePixel();
      applyColorToAll(gamma(map(pitchDiff, 0, 90, 0, 255)));                       // Alle Pixel blau je nach Fingerkrümmung (Differenz Pitch der beiden Sensoren)
      calcPixel(map(rollAbs, -45, 45, 1, 7), map(yawH, -180, 180, 0, 7));   // Roter Pixel je nach Rollposition, Grüner Pixel je nach Rotation
      showPixel();
    }

    void displayLoading(){
      for(int i = 0; i < 8; i++){
        if(i == currentLed){
          ledValsGreen[i] = 255;
        } else {
          ledValsGreen[i] = 0;
          ledValsRed[i] = 0;
          ledValsBlue[i] = 0;         // Blauer Pixel aus, wenn Grün an
        }
      }
      currentLed++;
      currentLed %= 8;
      showPixel();
      delay(50);
    }

    void initStrip(){
      strip.begin();
      strip.show();
      //onPixelRunColor(0,255,0);
    }

  private:
    void fadePixel(){
      for(int i=0; i<8; i++) {
        ledValsRed[i] = ledValsRed[i] - fadeVal;
        if (ledValsRed[i] <= 0){
          ledValsRed[i] = 0;
        }
        ledValsGreen[i] = ledValsGreen[i] - fadeVal;
        if (ledValsGreen[i] <= 0){
          ledValsGreen[i] = 0;
        }
        ledValsBlue[i] = ledValsBlue[i] - fadeVal;
        if (ledValsBlue[i] <= 0){
          ledValsBlue[i] = 0;
        }
      }
    }

    void applyColorToAll(int x){
      for(int i=0; i<8; i++) {
        ledValsBlue[i] = x;
      }
    }

    int gamma(int in){
      if(in > 255){
        in = 255;
      }else if(in < 0){
        in = 0;
      }
      float wert = (float)in / 255.0f;
      wert = pow(wert,2.35f);
      return (int)(wert*255.0f);
    }

    void calcPixel(int x, int y){
      ledValsRed[x] = 255;
      ledValsGreen[y] = 255;
      ledValsBlue[x] = 0;         // Blauer Pixel aus, wenn Rot an
      ledValsBlue[y] = 0;         // Blauer Pixel aus, wenn Grün an
    }

    void showPixel(){
      for(int i=0; i<8; i++) {
        strip.setPixelColor(i, strip.Color(ledValsRed[i], ledValsGreen[i], ledValsBlue[i]));  
      }
      strip.show();   
    }

    void onPixelRunColor(int x,int y,int z){
      for(int i=0; i<8; i++) {
        strip.setPixelColor(i, strip.Color(x, y, z));
        strip.show();
      }
    }
    
    private:
    // --- LED Ring --- //
    Adafruit_NeoPixel strip = Adafruit_NeoPixel(8, 3, NEO_GRB + NEO_KHZ800);
    int ledValsRed[8] = {0};      // Array für rote Pixel
    int ledValsGreen[8] = {0};    // Array für grüne Pixel
    int ledValsBlue[8] = {0};     // Array für blaue Pixel
    int fadeVal = 60;             // 0-255, 0 = immer an, 255 = kein Fade;
    int currentLed = 0;
};

#endif // PIXELRINGDISPLAY_H
