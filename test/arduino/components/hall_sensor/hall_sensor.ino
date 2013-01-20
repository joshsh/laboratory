/*
  Test program for Melexis US1881 Hall effect sensor
  
  Connect:
    sensor GND to GND
    sensor VCC to +5V
    sensor input pin to Arduino's D12
    Arduino's D8 to +5V via a 10k pullup resistor
    
  Latch and unlatch the sensor (turning the LED on and off) with the north
  and south poles of a magnet.
  Note: reportedly, very strong magnets should not be used with this sensor.
 */

const int sensorPin = 12;  
const int ledPin =  13;

void setup()
{
  pinMode(sensorPin, INPUT); 
  pinMode(ledPin, OUTPUT); 
  Serial.begin(9600);  
}

void loop()
{  
  int val = digitalRead(sensorPin);
  digitalWrite(ledPin, val);
}
