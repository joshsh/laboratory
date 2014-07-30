/*
  test program for the Breakout Board for Electret Microphone from SparkFun
       
  Connect the microphone in a pull-down configuration:
    GND pin to GND
    VCC pin to +5V
    AUD pin to Arduino's analog input 0
    1Mohm resistor from A1 to GND
    
  Note: I'm not sure if this is the best configuration... it's just what
  I came up with.
 */

const int sensorInputPin = A0;

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
  Serial.print(maxv);
  Serial.print("\t");
  Serial.println(maxv - minv); // volume
  //delay(300);
}
