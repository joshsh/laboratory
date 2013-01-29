/*
 * test program for flex sensor
 *
 * The component is said by Adafruit to have a resistance of around 10,000 ohm
 * when unflexed and around 20,000 ohm when flexed.
 *
 * Using a 10,000 ohm pullup resistor, the sensor reads:
 *     ~470 when flexed towards the bottom (non-text) side
 *     ~515 when unflexed
 *     ~720 when flexed towards the top (text) side
 *
 * Note: the spectrasymbol data sheet / advertisement doesn't indicate how far
 * you can safely bend the sensor.  However, it seems to survive a radius of
 * curvature of around 2.5cm in either direction.  The resistance for flexion
 * towards the bottom side levels off before it reaches that extreme, while
 * the resistance for flexion towards the top side seems to decrease without
 * bound.  After an extreme bend, the strip doesn't quite return to its
 * unflexed shape, and must be "helped".
 */

const int analogInputPin0 = 0;
const int analogInputPin1 = 1;
const int analogInputPin2 = 2;
const int analogInputPin3 = 3;
const int analogInputPin4 = 4;
const int analogInputPin5 = 5;

const int sensorInputPin = analogInputPin0;

void setup()
{
  Serial.begin(9600);
}

void loop()
{  
   int sensorValue = analogRead(sensorInputPin);   
   Serial.println(sensorValue); 
   delay(300);
}
