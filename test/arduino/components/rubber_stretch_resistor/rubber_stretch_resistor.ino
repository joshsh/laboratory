/*
 * test program for conductive rubber stretch sensor
 */

const int sensorInputPin = 0;
const int outPin = 2;

void setup()
{
  pinMode(outPin, OUTPUT);
  Serial.begin(115200);
  
  digitalWrite(outPin, HIGH);
}

// TODO: max int
int minVal;
int maxVal;
float range;

void resetRange() {
  minVal = 10000;
  maxVal = 0;
}

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
  resetRange();
  
  for (int i = 0; i < 100; i++) {
    int sensorValue = analogRead(sensorInputPin);   
    updateRange(sensorValue);
  }
    
  Serial.print("foo\t");
  Serial.print(millis());
  Serial.print("\t");
  Serial.print(minVal);
  Serial.print("\t");
  Serial.print(maxVal);
  Serial.println("");
}
