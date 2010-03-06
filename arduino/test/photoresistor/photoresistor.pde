/*
 * test program for photosensitive resistor
 *
 * The component is said by Adafruit to have a resistance of 5,000-10,000 ohm
 * in full light, 200,000 ohm in the dark.
 *
 * Using a 10,000 ohm pulldown resistor, the sensor reads:
 *     ~120 in a dimly-lit room, covered by a black cloth
 *     ~690 in typical artificial light
 *     ~880 in the beam of an LED flashlight
 */

const int analogInputPin0 = 0;
const int analogInputPin1 = 1;
const int analogInputPin2 = 2;
const int analogInputPin3 = 3;
const int analogInputPin4 = 4;
const int analogInputPin5 = 5;

const int photoSensorInputPin = analogInputPin0;

void setup()
{
  Serial.begin(9600);
}

void loop()
{  
   int sensorValue = analogRead(photoSensorInputPin);   
   Serial.println(sensorValue); 
}
