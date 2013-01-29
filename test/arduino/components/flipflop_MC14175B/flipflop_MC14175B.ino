/*
////////////////////////////////////////////////////////////////////////////////
MC14175B Quad Type D Flip-Flop -- test program

Setup:
* Vdd and Vss of the IC connected to 5V and ground, respectively
* pin #2 to Q output of one of the flip-flops
* pin #3 to Q' output of the same flip-flop
* D input of the same flip-flop connected by hand to 5V or ground
* clock input connected by hand to 5V or ground

Switch the clock and D inputs by hand, and observe the state of the flip-flop.


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

const int qPin = pin2;
const int qNaughtPin = pin3;

const int ledPin = pin13;

////////////////////////////////////////

void setup()
{
  Serial.begin(9600);
  
  pinMode(qPin, INPUT);
  pinMode(qNaughtPin, INPUT);
  
  pinMode(ledPin, OUTPUT);
}

void loop()
{  
  int q, qn;
  q = digitalRead(qPin);
  qn = digitalRead(qNaughtPin);
  
  digitalWrite(ledPin, q);
  
  Serial.print("Q: ");
  Serial.print(q);
  
  Serial.print("    Q': ");
  Serial.println(qn);

  delay(300);
}

