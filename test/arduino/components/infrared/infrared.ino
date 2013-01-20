/*
 * test program for infrared emitters and detectors (SparkFun SEN-00241)
 *
 * Followed this schematic:
 *     http://www.arduino.cc/cgi-bin/yabb2/YaBB.pl?num=1233960430/
 *
 */

const int analogInputPin0 = 0;
const int analogInputPin1 = 1;
const int analogInputPin2 = 2;
const int analogInputPin3 = 3;
const int analogInputPin4 = 4;
const int analogInputPin5 = 5;

const int ledPin = 11;

const int sensorInputPin = analogInputPin0;

void setup()
{
  Serial.begin(9600);
  
  pinMode(ledPin, OUTPUT);
}

const unsigned long samplesPerCycle = 10000;

const unsigned int rangeMin = 173, rangeMax = 858;

inline unsigned int analogOutValue(unsigned int v)
{
  return v < 0 ? 0 : v > 255 ? 255 : v;
}

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
     
    unsigned int outVal = 256 * ((double) (v - rangeMin) / (double) (rangeMax - rangeMin)); 
    analogWrite(ledPin, 255 - analogOutValue(outVal));
  }
  Serial.print(minv);
  Serial.print("\t");
  Serial.println(maxv);
  //delay(300);
}
