/*
 * Arduino performance and timing test.
 *
 * Results:
 *     ~200 digital reads/ms
 *     ~160 digital writes/ms
 *     ~1000 "pseudoflops"/ms. This is vague, but it indicates that arithmetic
 *     operations are only slightly faster than pin reads and writes.
 */
 
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

int inPin = 2;
int outPin = 3;

void setup()
{
  Serial.begin(9600);
  
  pinMode(inPin, INPUT);
  pinMode(outPin, OUTPUT);
}

unsigned long read_iters  = 200000;
unsigned long write_iters = 200000;
unsigned long cpu_iters   = 2000000;

int count = 0;

void loop()
{    
  unsigned long before, after;
  
  if (count < 10) {
    // digitalRead
    before = millis();
    for (unsigned long i = 0; i < read_iters; i++) {
      int x;
      
      x = digitalRead(inPin);
    }
    after = millis();
    Serial.print(read_iters / (after - before));
    Serial.println(" reads/ms");
    
    // digitalWrite
    before = millis();
    for (unsigned long i = 0; i < write_iters; i++) {
      digitalWrite(outPin, LOW);
    }
    after = millis();
    Serial.print(write_iters / (after - before));
    Serial.println(" writes/ms");
    
    // CPU
    int x = 7;
    before = millis();
    for (unsigned long i = 0; i < cpu_iters; i++) {
      x *= 3;
    }
    after = millis();
    Serial.print(cpu_iters / (after - before));
    Serial.print(" pseudoflops/ms");
    Serial.println(x);  // Disallow the compiler to optimize x away.
  }
  
  count++;
}
