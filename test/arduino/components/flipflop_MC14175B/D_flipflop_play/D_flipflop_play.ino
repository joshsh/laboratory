/*
////////////////////////////////////////////////////////////////////////////////
4-bit counter using MC14175B Quad Type D Flip-Flop

Setup:
* Vdd and Vss of the IC connected to 5V and ground, respectively
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

const int firstQPin = pin2;
const int totalQPins = 4;

const int clockPin = pin13;


////////////////////////////////////////

void setup()
{
  Serial.begin(9600);
  
  for (int i = 0; i < totalQPins; i++) {
  pinMode(firstQPin + i, INPUT);
  }
  
  pinMode(clockPin, OUTPUT);
}

int state = LOW;

void loop()
{  
  digitalWrite(clockPin, state);
  
  Serial.print(state);
  Serial.print("  ");
  
  for (int i = 0; i < totalQPins; i++) {
    Serial.print(digitalRead(firstQPin + i));
  }
  
  Serial.println("");

  delay(300);
  state = !state;
}

