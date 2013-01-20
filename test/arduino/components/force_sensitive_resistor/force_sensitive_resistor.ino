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

const int sensorInputPin = analogInputPin0;
const int outPin = 3;

void setup()
{
  pinMode(outPin, OUTPUT);
  Serial.begin(9600);
}

// TODO: max int
int minVal = 10000;
int maxVal = 0;
float range;

void updateRange(int val) {
   if (val < minVal) {
     minVal = val;
     range = 1 + maxVal - minVal;
   }
   if (val > maxVal) {
     maxVal = val;
     range = 1 + maxVal - minVal;
   }  
}

int count = 0;

void loop()
{  
   int sensorValue = analogRead(sensorInputPin);   
   updateRange(sensorValue);
   
   float outVal = 255.0 * (sensorValue - minVal) / range;
   outVal = constrain(outVal, 0, 255);
   analogWrite(outPin, (int) outVal);
    
   count = (count + 1) % 2000;
   if (0 == count) {
     Serial.println(sensorValue); 
   }
}
