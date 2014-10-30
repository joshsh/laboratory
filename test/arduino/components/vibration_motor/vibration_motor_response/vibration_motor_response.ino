/*
 * Latency/response program for the Precision Microdrives 10mm shaftless vibration motor
 * 
 * Connect one of the motor's two wires to pin 2, and the other to ground.
 * Hold the motor against your skin or teeth to feel,
 * hold against a piece of cardboard to hear, or
 * stand on edge or suspend from wire to observe visually.
 *
 * The motor is not *felt* to move at timesteps below 60ms or so, but it is *seen*
 * to move at as little as 10ms.
 */

const int timeStep = 75;
const int restStep = 1000;

const int motorPin1 = 2;
const int ledPin =  13;

void setup()
{
  pinMode(motorPin1, OUTPUT);
  pinMode(ledPin, OUTPUT); 
  Serial.begin(9600);  
}

void loop()
{  
    digitalWrite(motorPin1, HIGH);
    digitalWrite(ledPin, HIGH);
    delay(timeStep);
    
    digitalWrite(motorPin1, LOW);
    digitalWrite(ledPin, LOW);
    delay(restStep);
}
