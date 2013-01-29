/*
  test program for piezo vibration sensors
       
   Connect the sensor in a pull-down configuration:
     A1 to the sensor's "+" pin
     GND to the sensor's "-" pin
     1Mohm (or higher) resistor between A1 and GND
 
   Using a 4Mohm ohm pull-down resistor, the sensor reads:
     around 0-300 for vibration of a fist on a table
     up to 1023 when the sensor is flicked with a finger
      
   Note: using the Minisense 100 vibration sensor, this sketch caused my Arduino
   to lose its connection to the serial port (after a few lines of output),
   for unknown reasons.
   This did not occur when a piezo element was used instead.
 */

const int analogInputPin0 = 0;
const int analogInputPin1 = 1;
const int analogInputPin2 = 2;
const int analogInputPin3 = 3;
const int analogInputPin4 = 4;
const int analogInputPin5 = 5;

const int sensorInputPin = analogInputPin1;

void setup()
{
  Serial.begin(9600);
}

const unsigned long samplesPerCycle = 10000;

void loop()
{  
  unsigned int minv = 1000, maxv = 0;
  for (unsigned long i = 0; i < samplesPerCycle; i++)
  {
    unsigned int v = analogRead(sensorInputPin);
    if (v < minv)
    {
      minv = v;
    }
    
    else if (v > maxv)
    {
      maxv = v;
    }
  }
  Serial.print(minv);
  Serial.print("\t");
  Serial.println(maxv);
  //delay(300);
}
