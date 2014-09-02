/*
 * Test program for 5V piezo buzzers and piezo elements
 * 
 * Connect the piezo's leads to pin 2 and GND
 *
 * Note: all buzzers and speakers I have tried have "wrong" tones below
 * 40Hz or so.  This must be an Arduino artifact of some kind.
 */

const int speakerPin1 = 2;  
const int ledPin =  13;

void setup()
{
  pinMode(speakerPin1, OUTPUT); 
  pinMode(ledPin, OUTPUT); 
  Serial.begin(9600);  
}

const int tune[] = {1760, 880, 440, 220, 110, 55, 26};
const int tuneLength = 7;

//const int tune[] = {100, 90, 80, 70, 60, 50, 40, 30, 20};
//const int tuneLength = 9;

const int time = 1000;

void loop()
{  
  int i;
  for (int i = 0; i < tuneLength; i++) {
    tone(speakerPin1, tune[i]);
    Serial.print("playing "); Serial.println(tune[i]);
    delay(time);
  }
}
