/*
 * test program for PIR motion sensor
 */
 
int pirPin = 8;

void setup(){
   Serial.begin(9600);
   pinMode(pirPin, INPUT);
}

int count = 0;

void loop(){
    int pirVal = digitalRead(pirPin);
    if(pirVal == LOW){ //was motion detected
        Serial.print(++count);
        Serial.println(")\tmotion detected");
        delay(1000);
    }
}
