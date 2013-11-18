/*
  Test program for a color LED.
  
  For use with the BL-L515 Round Type, FULL COLOR LED lamp (common anode).
  Connect R, G, B pins of the LED to (PWM) digital out pins 9, 10, and 11,
  respectively, and connect the anode (long pin) to ground through a
  100Ω resistor.
  The green and blue pins of the LED may be reversed w.r.t. the datasheet.
  Note: this LED is more commonly wired with separate resistors for each of
  the color pins (rather than a single resistor for the anode).
  The one-resistor configuration simpler, although technically it exposes
  the red LED component to higher than recommended voltage.  It also requires
  you to compensate for the "overpowering" red in the code (see RED_FACTOR).
 */

#include <stdlib.h>

const int redPin = 9;
const int greenPin = 10;
const int bluePin = 11;

const unsigned long WHITE = 0xffffff;
const unsigned long RED = 0xff0000;
const unsigned long YELLOW = 0xffff00;
const unsigned long GREEN = 0x00ff00;
const unsigned long CYAN = 0x00ffff;
const unsigned long BLUE = 0x0000ff;
const unsigned long PURPLE = 0xff00ff;
const unsigned long BLACK = 0x000000;

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 100;

////////////////////////////////////////

void writeColor(unsigned long color)
{
  unsigned long red = (color & RED) >> 16;
  unsigned long green = (color & GREEN) >> 8;
  unsigned long blue = (color & BLUE);
  
  red = (red * RED_FACTOR) / 255;
  
  analogWrite(redPin, 255 - (unsigned int) red);
  analogWrite(greenPin, 255 - (unsigned int) green);
  analogWrite(bluePin, 255 - (unsigned int) blue);
}

void setup()
{    
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);
}

void loop()
{      
  for (int i=0; i < 10; i++) {
    writeColor(WHITE);
    delay(50);
    writeColor(BLACK);
    delay(50);
  }
  
  writeColor(WHITE);
  delay(1000);
  writeColor(RED);
  delay(1000);
  writeColor(YELLOW);
  delay(1000);
  writeColor(GREEN);
  delay(1000);
  writeColor(CYAN);
  delay(1000);
  writeColor(BLUE);
  delay(1000);
  writeColor(PURPLE);
  delay(1000);
  writeColor(BLACK);
  delay(1000);
}
