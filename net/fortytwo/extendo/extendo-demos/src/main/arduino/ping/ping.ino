#define EXCEPTION 'x'
#define PING      'p'
#define REPLY     'r'

void setup() {
    Serial.begin(115200);
    //SLIPSerial.begin(38400);  // works equally well with 8MHz and 16MHz
#if ARDUINO >= 100
    while (!Serial); // for Arduino Leonardo
#endif  
}

void loop() {
    if (Serial.available()) {
        int b = Serial.read();
        if (PING == b) {
            Serial.print(REPLY);
        } else {
            Serial.print(EXCEPTION);
        }
    }
}

