/*
 * test program for the Minisense 100 piezo vibration sensor
 *
 * Seeing no other instructions, I've connected the sensor in a pull-down
 * configuration.
 *
 *
 * Using a 10,000 ohm pull-down resistor, the sensor reads:
 *     around 110 - 116 in rest
 *     around 64  - 148 when flicked with a finger
 *
 * Unfortunately for my intended application, the sensor does not seem to
 * respond to slight movement, but only to forceful vibration, as on impact.
 *
 * The output range of the sensor also seems to wander somewhat, for unknown
 * reasons.  Did I damage it by flicking it too hard? 
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
