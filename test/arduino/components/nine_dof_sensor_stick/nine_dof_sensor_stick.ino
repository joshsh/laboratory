/*
Test program for Sparkfun's 9 Degrees of Freedom - Sensor Stick, SEN-10724
Uses I2Cdevlib by Jeff Rowberg

Created by Joshua Shinavier, 2014
Released into the public domain
*/

#include <Wire.h>
#include <I2Cdev.h>
#include <ADXL345.h>
#include <ITG3200.h>
#include <HMC5883L.h>

ADXL345 accel;
ITG3200 gyro;
HMC5883L mag;

#define SEP '\t'
//#define SEP ','

void setup() {
    // join I2C bus (I2Cdev library doesn't do this automatically)
    Wire.begin();

    Serial.begin(38400);
    
#if ARDUINO >= 100
    while (!Serial); // for Arduino Leonardo
#endif

    // adjust the power settings after you call this method if you want the accelerometer
    // to enter standby mode, or another less demanding mode of operation
    accel.initialize();
    if (!accel.testConnection()) {
        Serial.println("ADXL345 connection failed");
    }
    
    gyro.initialize();
    if (!accel.testConnection()) {
        Serial.println("ITG3200 connection failed");
    }
    
    mag.initialize();
    if (!accel.testConnection()) {
        Serial.println("HMC5883L connection failed");
    }
}

void loop() {
  
  Serial.print(micros());
  
  // ADXL345 accelerometer /////////////////////////////////////////////////////

  /*
    Serial.print("offset:");
    Serial.print("\t"); Serial.print(accel.getOffsetX());
    Serial.print("\t"); Serial.print(accel.getOffsetY());
    Serial.print("\t"); Serial.print(accel.getOffsetZ());
    Serial.print("\n");
    */
    
    /*
    Serial.print("tap threshold:\t"); Serial.println(accel.getTapThreshold());
    Serial.print("tap duration:\t"); Serial.println(accel.getTapDuration());
    Serial.print("double tap latency:\t"); Serial.println(accel.getDoubleTapLatency());
    Serial.print("double tap window:\t"); Serial.println(accel.getDoubleTapWindow());
    Serial.print("inactivity threshold:\t"); Serial.println(accel.getInactivityThreshold());
    Serial.print("inactivity time:\t"); Serial.println(accel.getInactivityTime());
    Serial.print("free fall threshold:\t"); Serial.println(accel.getFreefallThreshold());
    Serial.print("free fall time:\t"); Serial.println(accel.getFreefallTime());
    //...
    Serial.print("rate:\t"); Serial.println(accel.getRate());
    */
    
    int16_t ax, ay, az;
    accel.getAcceleration(&ax, &ay, &az);
    Serial.print(SEP); Serial.print("accel");
    Serial.print(SEP); Serial.print(ax);
    Serial.print(SEP); Serial.print(ay);
    Serial.print(SEP); Serial.print(az);
    
    // ITG3200 gyroscope ///////////////////////////////////////////////////////

    int16_t gx, gy, gz;
    gyro.getRotation(&gx, &gy, &gz);
    Serial.print(SEP); Serial.print("gyro");
    Serial.print(SEP); Serial.print(gx);
    Serial.print(SEP); Serial.print(gy);
    Serial.print(SEP); Serial.print(gz);
    
    // HMC5883L magnetometer ///////////////////////////////////////////////////

    int16_t mx, my, mz;
    mag.getHeading(&mx, &my, &mz);
    Serial.print(SEP); Serial.print("mag");
    Serial.print(SEP); Serial.print(mx);
    Serial.print(SEP); Serial.print(my);
    Serial.print(SEP); Serial.print(mz);
    
    float heading = atan2(my, mx);
    if(heading < 0)
        heading += 2 * M_PI;
    Serial.print(SEP); Serial.print("heading");
    Serial.print(SEP); Serial.print(heading * 180/M_PI);
    
    ////////////////////////////////////////////////////////////////////////////

    Serial.print("\n");
    delay(100);
}

