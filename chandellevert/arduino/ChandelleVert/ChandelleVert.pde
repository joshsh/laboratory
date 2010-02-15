/*
 * Switch test program
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

int sensorPin = pin3;
int outPin =  pin13;

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

int lineno = 0;

////////////////////////////////////////

int muxSelectPins[] = {pin4, pin5, pin6, pin7};

#define MUX_LEVELS 1
#if MUX_LEVELS == 1
  #define SELECT_PINS 4
  #define BITS_PER_SAMPLE 16
#elif MUX_LEVELS == 2
  #define SELECT_PINS 8
  #define BITS_PER_SAMPLE 256
#elif MUX_LEVELS == 3
  #define SELECT_PINS 12
  #define BITS_PER_SAMPLE 4096
#endif

#define SAMPLING_DELAY 0

#define SIMULATE_RESPONSIVENESS
#ifdef SIMULATE_RESPONSIVENESS
  #define SAMPLES_PER_CYCLE 64 // 64 MUXes for 1024 sensors
#else
  #define SAMPLES_PER_CYCLE 16
#endif

unsigned char sampleData[BITS_PER_SAMPLE / 8]; 
unsigned char prevSampleData[BITS_PER_SAMPLE / 8];

#define PRETTY_MODE


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

unsigned int muxSelectPinsState[SELECT_PINS];

inline void setMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(muxSelectPins[j], val); 
  muxSelectPinsState[j] = val;
}

inline void sampleSensor(unsigned int i)
{
  unsigned int x = !digitalRead(sensorPin);
  // TODO
  sampleData[i / 8] |= (x << (i % 8));
  //if (x) Serial.println("\tGot one!");
  //if (x) { Serial.println((unsigned int) sampleData[i/8]); }
}

int hasChanged;

void advanceSampleData()
{
  hasChanged = false;
  
  unsigned char *cur = sampleData;
  unsigned char *prev = prevSampleData;
  
  while (cur < sampleData + BITS_PER_SAMPLE / 8)
  {
    if (*cur != *prev)
    {
      hasChanged = true;
    }
    
    *prev = *cur;
    *cur = 0;
    
    cur++;
    prev++;
  }
}

void sample_new()
{
  for (int k = 0; k < SELECT_PINS; k++)
  {
    setMuxSelectPin(k, 0);
  }

  int i = 0;
  //Serial.println("Sampling...");
  while (i < BITS_PER_SAMPLE)
  {
    /*
    Serial.print("\t\t");
    for (int k = 0; k < SELECT_PINS; k++)
    {
      Serial.print(muxSelectPinsState[k]);
    }
    Serial.println("");*/
    
    if (i > 0)
    {
      int j = 0;
      while (muxSelectPinsState[j])
      {
        setMuxSelectPin(j, 0);
        j++;
      } 
      setMuxSelectPin(j, 1);
    }
    
    sampleSensor(i);
    i++;
    setMuxSelectPin(0, 1);
    sampleSensor(i);
    i++;
  }
}

void outputSampleData()
{
#ifdef PRETTY_MODE
  lineno++;

  Serial.print(lineno);
  Serial.print(") ");
  for (int i = 0; i < BITS_PER_SAMPLE; i++)
  {
    int b = prevSampleData[i / 8] & (1 << (i % 8));
    Serial.print(b ? 'o' : ' ');
  }
#else
  for (int l = 0; l < BITS_PER_SAMPLE / 8; l++)
  {
    Serial.print(prevSampleData[l]);      
  }
#endif

  Serial.println("");
}

/*
unsigned int sample()
{
  unsigned int activeSensors = 0;
  
  for (int s = 0; s < SAMPLES_PER_CYCLE; s++)
  {
    for (unsigned int i = 0; i < 16; i++)
    {
      for (unsigned int j = 0; j < 4; j++)
      {
        //Serial.print("\t\t");
        //Serial.print("i: ");
        //Serial.print(i);
        //Serial.print(" --> ");
        //Serial.println((i >> j) & 1);
        digitalWrite(muxSelectPins[j], i >> j & 1 ? HIGH : LOW);  
      }
      

      if (SAMPLING_DELAY)
      {
        delay(SAMPLING_DELAY);
      }
      
      int x = !digitalRead(sensorPin);
 #ifdef SIMULATE_RESPONSIVENESS
      if (0 == s)
 #endif
      activeSensors += (x << i);
      
      
      if (SAMPLING_DELAY)
      {
        delay(SAMPLING_DELAY);
      }
    }
  }
  
  return activeSensors;
}
*/

void setup()
{
  Serial.begin(9600);           // set up Serial library at 9600 bps
  pinMode(sensorPin, INPUT);
  pinMode(outPin, OUTPUT);
  
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);
  
  for (int i = 0; i < SELECT_PINS; i++)
  {
    pinMode(muxSelectPins[i], OUTPUT);
  }
  
  // Trick to force outputting of an artificial blank sensor map.
  prevSampleData[0] = 1;
}

//unsigned int last_activeSensors = 0;

void loop()
{    
  advanceSampleData();
  
  if (hasChanged)
  {
      //digitalWrite(outPin, activeSensors ? HIGH : LOW);

    outputSampleData();
/*
      lineno++;
      
      Serial.print(lineno);
      Serial.print(") ");
      Serial.println(activeSensors);    // Read the pin and display the value
*/
  }

//  unsigned int activeSensors = sample();

  for (int s = 0; s < SAMPLES_PER_CYCLE; s++)
  {
    sample_new();
  }

  // Reporting a sample only when the sample changes makes the device much more
  // responsive.
//  if (activeSensors != last_activeSensors)

  
  //last_activeSensors = activeSensors; 
  
#ifdef EYE_CANDY
  // Mere eye candy
  //writeColor(colors[(lineno / 3) % COLORS]);
  nextColor();
#endif
}
