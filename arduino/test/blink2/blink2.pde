/*
  Blink
 
 Turns on an LED on for one second, then off for one second, repeatedly.
 
 The circuit:
 * LED connected from digital pin 13 to ground.
 
 * Note: On most Arduino boards, there is already an LED on the board
 connected to pin 13, so you don't need any extra components for this example.
 
 
 Created 1 June 2005
 By David Cuartielles
 
 http://arduino.cc/en/Tutorial/Blink
 
 based on an orginal by H. Barragan for the Wiring i/o board
 
 */

int ledPin1 =  13;    // LED connected to digital pin 13
int ledPin2 =  12;    // LED connected to digital pin 12

// The setup() method runs once, when the sketch starts

void setup()   {                
  // initialize the digital pin as an output:
  pinMode(ledPin1, OUTPUT); 
  pinMode(ledPin2, OUTPUT);
}

// the loop() method runs over and over again,
// as long as the Arduino has power

void on(int ledPin, int howlong)
{
  digitalWrite(ledPin, HIGH);
  delay(howlong);
}

void off(int ledPin, int howlong)
{
  digitalWrite(ledPin, LOW);
  delay(howlong);
}

void first(int howlong)
{
  digitalWrite(ledPin1, HIGH);
  digitalWrite(ledPin2, LOW);
  delay(howlong);
}

void second(int howlong)
{
  digitalWrite(ledPin1, LOW);
  digitalWrite(ledPin2, HIGH);
  delay(howlong);
}

void loop()                     
{
  first(50);
  second(200);
  first(50);
  second(200);
  first(50);
  second(450);
  /*
  digitalWrite(ledPin, HIGH);   // set the LED on
  delay(1000);                  // wait for a second
  digitalWrite(ledPin, LOW);    // set the LED off
  delay(1000);                  // wait for a second
  */
}
