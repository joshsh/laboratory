int dustPin = A3;
int dustVal = 0;
 
int ledPower = 3;
int delayTime=280;
int delayTime2=40;
float offTime=9680;

void setup(){
Serial.begin(9600);
pinMode(ledPower,OUTPUT);
//pinMode(4, OUTPUT);
}
 
void loop(){
// ledPower is any digital pin on the arduino connected to Pin 3 on the sensor
digitalWrite(ledPower,LOW); // power on the LED
delayMicroseconds(delayTime);
dustVal=analogRead(dustPin); // read the dust value via pin 5 on the sensor
delayMicroseconds(delayTime2);  // what is the purpose of the second delay?
digitalWrite(ledPower,HIGH); // turn the LED off
delayMicroseconds(offTime);  // just to make it an even 10ms, apparently
 
delay(1000);
Serial.println(dustVal);
}
