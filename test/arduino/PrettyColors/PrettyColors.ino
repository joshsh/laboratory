/*
  Pretty colors.
  
  For use with the BL-L515 Round Type, FULL COLOR LED lamp (common anode)p
  Connect G, B, R pins of the LED to (PWM) digital out pins 9, 10, and 11,
  respectively, and connect the anode (long pin) to ground through a
  100Ω resistor.
 */

#include <stdlib.h>

const int redPin = 11;
const int greenPin = 9;
const int bluePin = 10;

const double SPEED_LIMIT = 0.001;
const double ACCELERATION = 0.000125;
//#define INSTABILITY 0.01

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 60;

////////////////////////////////////////

void writeColor(unsigned long color)
{
  unsigned int red = (color & 0xff0000) >> 16;
  unsigned int green = (color & 0x00ff00) >> 8;
  unsigned int blue = (color & 0x0000ff);
  
  red = (red * RED_FACTOR) / 255;
  
  analogWrite(redPin, 255 - red);
  analogWrite(greenPin, 255 - green);
  analogWrite(bluePin, 255 - blue);
}

double x = 0.0, y = 0.0, z = 0.0;
double xspeed = 0.0, yspeed = 0.0, zspeed = 0.0;
//float theta = 0.0, phi = 0.0;

unsigned long m_w = 42;    /* must not be zero */
unsigned long m_z = 1331;    /* must not be zero */
 
unsigned long get_random()
{
    m_z = 36969 * (m_z & 65535) + (m_z >> 16);
    m_w = 18000 * (m_w & 65535) + (m_w >> 16);
    return (m_z << 16) + m_w;  /* 32-bit result */
}

double floatRand(double mn, double sup)
{
  return mn + (get_random() % RAND_MAX) * (sup - mn) / ((double) RAND_MAX + 1);
}

void nextColor() {
  double xacc = floatRand(-ACCELERATION, ACCELERATION);
  double yacc = floatRand(-ACCELERATION, ACCELERATION);
  double zacc = floatRand(-ACCELERATION, ACCELERATION);

  xspeed += xacc;
  yspeed += yacc;
  zspeed += zacc;
  
  if (xspeed > SPEED_LIMIT)
  {
    xspeed = SPEED_LIMIT;
  }
  
  else if (xspeed < -SPEED_LIMIT)
  {
    xspeed = -SPEED_LIMIT;
  }
  
  if (yspeed > SPEED_LIMIT)
  {
    yspeed = SPEED_LIMIT;
  }
  
  else if (yspeed < -SPEED_LIMIT)
  {
    yspeed = -SPEED_LIMIT;
  }
    
  if (zspeed > SPEED_LIMIT)
  {
    zspeed = SPEED_LIMIT;
  }
  
  else if (zspeed < -SPEED_LIMIT)
  {
    zspeed = -SPEED_LIMIT;
  }
  
  x += xspeed;
  y += yspeed;
  z += zspeed;
  
  if (x > 1.0)
  {
    x = 2.0 - x;
    xspeed = -xspeed;
  }
  
  else if (x < 0.0)
  {
    x = -x;
    xspeed = -xspeed;
  }
  
  if (y > 1.0)
  {
    y = 2.0 - y;
    yspeed = -yspeed;
  }
  
  else if (y < 0.0)
  {
    y = -y;
    yspeed = -yspeed;
  }
  
  if (z > 1.0)
  {
    z = 2.0 - z;
    zspeed = -zspeed;
  }
  
  else if (z < 0.0)
  {
    z = -z;
    zspeed = -zspeed;
  }

  unsigned int red = x * 255;
  unsigned int green = y * 255;
  unsigned int blue = z * 255;
  
  unsigned long color = red * (unsigned long) 0x10000
    + green * 0x100
    + blue;
    
  writeColor(color); 
}

////////////////////////////////////////

void setup()
{  
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);
}

void loop()
{  
  nextColor();
  delay(1);
}
