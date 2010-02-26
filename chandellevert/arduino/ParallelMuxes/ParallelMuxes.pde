/*
 * ChandelleVert test program
 */

#include <stdlib.h>

int pin0 = 0;    // RX
int pin1 = 1;    // TX
int pin2 = 2;    // digital 
int pin3 = 3;    // digital (PWM)
int pin4 = 4;    // digital
int pin5 = 5;    // digital (PWM)
int pin6 = 6;    // digital (PWM)
int pin7 = 7;    // digital
int pin8 = 8;    // digital (PWM)
int pin9 = 9;    // digital (PWM)
int pin10= 10;   // digital (PWM)
int pin11 = 11;  // digital
int pin12 = 12;  // digital
int pin13 = 13;  // digital
int pin14 = 14;  // analog in 0
int pin15 = 15;  // analog in 1
int pin16 = 16;  // analog in 2
int pin17 = 17;  // analog in 3
int pin18 = 18;  // analog in 4
int pin19 = 19;  // analog in 5

int amuxInputPin = pin15;
int bmuxInputPin = pin6;

int amuxSelectPins[] = {pin16, pin17, pin18, pin19};
int bmuxSelectPins[] = {pin2, pin3, pin4, pin5};

const unsigned int INVERT_BMUX_SELECT = true;

int lineno = 0;

////////////////////////////////////////

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

#define SENSOR_MULTIPLIER 1

unsigned char sampleData[BITS_PER_SAMPLE / 8]; 
unsigned char prevSampleData[BITS_PER_SAMPLE / 8];

#define PRETTY_MODE

////////////////////////////////////////

unsigned int amuxSelectPinsState[SELECT_PINS];
unsigned int bmuxSelectPinsState[SELECT_PINS];

inline void setAMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(amuxSelectPins[j], val); 
  amuxSelectPinsState[j] = val;
}

inline void setBMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(bmuxSelectPins[j], INVERT_BMUX_SELECT ? !val : val); 
  bmuxSelectPinsState[j] = val;
}

inline void sampleSensor(unsigned int i)
{
  unsigned int x = !digitalRead(amuxInputPin);
  // TODO
  sampleData[i / 8] |= (x << (i % 8));
  //if (x) Serial.println("\tGot one!");
  //if (x) { Serial.println((unsigned int) sampleData[i/8]); }
}

//unsigned int count;

inline void sampleSensorParallel(unsigned int m)
{
  unsigned int x;
  unsigned int mi = m / 8;
  unsigned int mj = m % 8;
  
  for (unsigned int q = 0; q < SENSOR_MULTIPLIER; q++) {
    for (unsigned int k = 0; k < SELECT_PINS; k++)
    {
      setBMuxSelectPin(k, false);
    }
  
    unsigned int i = 0;
    while (i < BITS_PER_SAMPLE)
    {
      if (i > 0)
      {
        unsigned int j = 0;
        while (bmuxSelectPinsState[j])
        {
          setBMuxSelectPin(j, false);
          j++;
        } 
        setBMuxSelectPin(j, true);
      }
      
      x = !digitalRead(amuxInputPin);
      sampleData[mi] |= (x << mj);
  //    count++;
      i++;
      
      setBMuxSelectPin(0, true);
      x = !digitalRead(amuxInputPin);
      sampleData[mi] |= (x << mj);
  //    count++;
      i++;
    }
  }
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

void sampleSimple()
{
  for (int k = 0; k < SELECT_PINS; k++)
  {
    setAMuxSelectPin(k, false);
  }

  int i = 0;
  //Serial.println("Sampling...");
  while (i < BITS_PER_SAMPLE)
  {
    /*
    Serial.print("\t\t");
    for (int k = 0; k < SELECT_PINS; k++)
    {
      Serial.print(amuxSelectPinsState[k]);
    }
    Serial.println("");*/
    
    if (i > 0)
    {
      int j = 0;
      while (amuxSelectPinsState[j])
      {
        setAMuxSelectPin(j, false);
        j++;
      } 
      setAMuxSelectPin(j, true);
    }
    
    sampleSensor(i);
    i++;
    setAMuxSelectPin(0, true);
    sampleSensor(i);
    i++;
  }
}

void sampleParallel()
{
  for (unsigned int q = 0; q < SENSOR_MULTIPLIER; q++) {
    for (int k = 0; k < SELECT_PINS; k++)
    {
      setAMuxSelectPin(k, false);
    }
  
    int i = 0;
    //Serial.println("Sampling...");
    while (i < BITS_PER_SAMPLE)
    {
      /*
      Serial.print("\t\t");
      for (int k = 0; k < SELECT_PINS; k++)
      {
        Serial.print(amuxSelectPinsState[k]);
      }
      Serial.println("");*/
      
      if (i > 0)
      {
        int j = 0;
        while (amuxSelectPinsState[j])
        {
          setAMuxSelectPin(j, false);
          j++;
        } 
        setAMuxSelectPin(j, true);
      }
      
      sampleSensorParallel(i);
      i++;
      setAMuxSelectPin(0, true);
      sampleSensorParallel(i);
      i++;
    }
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

void setup()
{
  // set up Serial library at 9600 bps
  Serial.begin(9600);
  
  pinMode(amuxInputPin, INPUT);
  pinMode(bmuxInputPin, INPUT);
  
  for (int i = 0; i < SELECT_PINS; i++)
  {
    pinMode(amuxSelectPins[i], OUTPUT);
    pinMode(bmuxSelectPins[i], OUTPUT);
  }
  
  // Trick to force outputting of an artificial blank sensor map.
  prevSampleData[0] = 1;
}

void loop()
{    
  advanceSampleData();
  
  if (hasChanged)
  {
    outputSampleData();
  }

  //count = 0;
  //sampleSimple();
  sampleParallel();
  //Serial.println(count);
}
