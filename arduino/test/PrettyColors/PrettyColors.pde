/*
 * Pretty colors.
 */

#include <stdlib.h>

int pin0 = 0;
int pin1 = 1;
int pin2 = 2;
int pin3 = 3;
int pin4 = 4;
int pin5 = 5;
int pin6 = 6;
int pin7 = 7;
int pin8 = 8;
int pin9 = 9;
int pin10= 10;
int pin11 = 11;
int pin12 = 12;
int pin13 = 13;

int redPin = pin11;
int greenPin = pin9;
int bluePin = pin10;

int BLACK = 0;
int BLUE = 1;
int GREEN = 2;
int CYAN = 3;
int RED = 4;
int PURPLE = 5;
int YELLOW = 6;
int WHITE = 7;

////////////////////////////////////////

void writeColor(unsigned long color)
{
  unsigned int red = (color & 0xff0000) >> 16;
  unsigned int green = (color & 0x00ff00) >> 8;
  unsigned int blue = (color & 0x0000ff);
  
  analogWrite(redPin, 255 - red);
  analogWrite(greenPin, 255 - green);
  analogWrite(bluePin, 255 - blue);
}

double SPEED_LIMIT = 0.1;
double ACCELERATION = 0.0125;
//#define INSTABILITY 0.01

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

//#define EYE_CANDY

unsigned long colors[]
  = {0xff0000, 0x00ff00, 0x0000ff, 0xffff00, 0xff8000, 0x000000};
#define COLORS 6

////////////////////////////////////////

void setup()
{
  Serial.begin(9600);           // set up Serial library at 9600 bps
  
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);
}

void loop()
{    
  nextColor();
  delay(100);
}
