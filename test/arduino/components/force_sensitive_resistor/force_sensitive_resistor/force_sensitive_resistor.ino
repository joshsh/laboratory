/*
 * test program for force sensitive resistor
 *
 * The component is said by Adafruit to have a resistance which ranges from
 * infinity down to around 250 ohm (at 22 pounds pressure).
 *
 * Using a 10,000 ohm pullup resistor, the sensor reads:
 *     ~904 when unpressed (same as that of an infinite resistor)
 *     ~150 when pressed hard between thumb and forefinger
 */

const int analogInputPin0 = 0;
const int analogInputPin1 = 1;
const int analogInputPin2 = 2;
const int analogInputPin3 = 3;
const int analogInputPin4 = 4;
const int analogInputPin5 = 5;

int ledPin1 =  11;    // LED connected to digital pin 11

const int sensorInputPin = analogInputPin0;

void setup()
{
  pinMode(ledPin1, OUTPUT); 
  Serial.begin(9600);
}

void loop()
{  
  int v;
  
   int sensorValue = analogRead(sensorInputPin);   
   Serial.println(sensorValue);
   
   v = (int) (256 * ((double) sensorValue) / 1000.0);
   analogWrite(ledPin1, v);
   
   delay(300);
}

