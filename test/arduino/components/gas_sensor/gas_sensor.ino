int sensorValue;

void setup()
{
  Serial.begin(9600);      // sets the serial port to 9600
}

void loop()
{
  int minv = 1024;
  int maxv = 0;
  for (int i = 0; i < 10; i++) {
    sensorValue = analogRead(0);       // read analog input pin 0
    if (sensorValue < minv) {
      minv = sensorValue;
    }
    if (sensorValue > maxv) {
      maxv = sensorValue;
    }
    delay(100);                        // wait 100ms for next reading
  }
  Serial.print(minv, DEC);  // prints the value read
  Serial.print("\t");
  Serial.println(maxv, DEC);
}
