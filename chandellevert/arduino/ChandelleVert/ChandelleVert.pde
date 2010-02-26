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

int outPin =  pin13;

int sensorPin = pin15;
//int sensorPin = 6;

int muxSelectPins1[] = {pin16, pin17, pin18, pin19};
//int muxSelectPins1[] = {pin2, pin3, pin4, pin5};

const unsigned int REVERSE = false;
//const unsigned int REVERSE = true;

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

unsigned int muxSelectPins1State[SELECT_PINS];

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

inline void setMuxSelectPin(unsigned int j, unsigned int val)
{
  digitalWrite(muxSelectPins1[j], REVERSE ? !val : val); 
  muxSelectPins1State[j] = val;
}

void sampleSensors()
{
  for (int k = 0; k < SELECT_PINS; k++)
  {
    setMuxSelectPin(k, false);
  }

  int i = 0;
  //Serial.println("Sampling...");
  while (i < BITS_PER_SAMPLE)
  {
    /*
    Serial.print("\t\t");
    for (int k = 0; k < SELECT_PINS; k++)
    {
      Serial.print(muxSelectPins1State[k]);
    }
    Serial.println("");*/
    
    if (i > 0)
    {
      int j = 0;
      while (muxSelectPins1State[j])
      {
        setMuxSelectPin(j, false);
        j++;
      } 
      setMuxSelectPin(j, true);
    }
    
    sampleSensor(i);
    i++;
    setMuxSelectPin(0, true);
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

void setup()
{
  Serial.begin(9600);           // set up Serial library at 9600 bps
  pinMode(sensorPin, INPUT);
  pinMode(outPin, OUTPUT);
  
  for (int i = 0; i < SELECT_PINS; i++)
  {
    pinMode(muxSelectPins1[i], OUTPUT);
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

  for (int s = 0; s < SAMPLES_PER_CYCLE; s++)
  {
    sampleSensors();
  }
}
