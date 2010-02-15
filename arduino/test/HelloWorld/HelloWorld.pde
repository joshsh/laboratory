/*
 * Hello World!
 *
 * This is the Hello World! for Arduino. 
 * It shows how to send data to the computer
 */

void setup()                    // run once, when the sketch starts
{
  Serial.begin(9600);           // set up Serial library at 9600 bps
  
  Serial.println("Hello, sailor!");  // prints hello with ending line break 
}

void loop()                       // run over and over again
{
  Serial.println("Beep.");  // prints hello with ending line break
  delay(1000);
}
