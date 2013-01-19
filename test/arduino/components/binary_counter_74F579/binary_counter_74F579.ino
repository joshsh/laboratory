/*
////////////////////////////////////////////////////////////////////////////////
74F579 8-bit bidirectional binary counter -- test program

...

Setup:
* ...

////////////////////////////////////////////////////////////////////////////////
*/

#include <stdlib.h>

const int pin0 = 0;    // RX
const int pin1 = 1;    // TX
const int pin2 = 2;    // digital 
const int pin3 = 3;    // digital (PWM)
const int pin4 = 4;    // digital
const int pin5 = 5;    // digital (PWM)
const int pin6 = 6;    // digital (PWM)
const int pin7 = 7;    // digital
const int pin8 = 8;    // digital (PWM)
const int pin9 = 9;    // digital (PWM)
const int pin10= 10;   // digital (PWM)
const int pin11 = 11;  // digital
const int pin12 = 12;  // digital
const int pin13 = 13;  // digital
const int pin14 = 14;  // analog in 0
const int pin15 = 15;  // analog in 1
const int pin16 = 16;  // analog in 2
const int pin17 = 17;  // analog in 3
const int pin18 = 18;  // analog in 4
const int pin19 = 19;  // analog in 5


////////////////////////////////////////

const int firstIoPin = pin2;
const int totalIoPins = 8;

const int clockPin = pin13;

int ioPinValues[totalIoPins];

void readIoPinValues() {
  for (int i = 0; i < totalIoPins; i++) {
    ioPinValues[i] = digitalRead(firstIoPin + i);
  }
}


////////////////////////////////////////

void setup()
{
  Serial.begin(9600);
  
  for (int i = 0; i < totalIoPins; i++) {
    pinMode(firstIoPin + i, INPUT);
  }
  
  pinMode(clockPin, OUTPUT);
}

int state = HIGH;

void loop()
{  
  digitalWrite(clockPin, state);
  
  readIoPinValues();
  for (int i = 0; i < totalIoPins; i++) {
    Serial.print(ioPinValues[i]);
  }
  for (int i = 0; i < 6; i++) {
    Serial.print(" ");
    Serial.print(analogRead(i));
  }
  Serial.println("");
  
  state = !state;
  //delay(300);
}

