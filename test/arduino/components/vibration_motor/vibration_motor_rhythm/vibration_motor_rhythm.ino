/*
 * Test program for the Precision Microdrives 10mm shaftless vibration motor
 * 
 * Connect one of the motor's two wires to pin 2, and the other to ground.
 * Hold the motor against your skin (to feel) or against a piece of cardboard
 * (to hear).
 *
 * Play with the value of timeStep:
 *   t = 100 -- rhythm can be *felt* very clearly, anywhere on the body
 *   t = 50 -- rhythm can still be felt, if you know what to "feel" for
 *   t = 40 -- rhythm is difficult to feel correctly, but sounds perfect
 *   t = 30 -- sounds less than perfect. Motor problem or sensory problem?
 *   t = 20 -- the intended rhythm is no longer discernable
 *
 * Interestingly, visual rhythm is even less sensitive: even at t = 100,
 * it can be hard to follow the rhythm of the LED.  When you then touch the
 * motor or allow yourself to hear the motor, you "get it".
 */

const int timeStep = 100;

const int motorPin1 = 2;
const int ledPin =  13;

void setup()
{
  pinMode(motorPin1, OUTPUT);
  pinMode(ledPin, OUTPUT); 
  Serial.begin(9600);  
}

const char* rhythm = "--__-_-_--__-_-_--__--__----____"; 
const int length = 32;

void loop()
{  
  for (int i = 0; i < length; i++) {
    if (rhythm[i] == '-') {
      digitalWrite(motorPin1, HIGH);
      digitalWrite(ledPin, HIGH);
    } else {
      digitalWrite(motorPin1, LOW);
      digitalWrite(ledPin, LOW);
    }
    delay(timeStep);
  }
}
