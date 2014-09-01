#define LEONARDO_SERIAL

#ifdef LEONARDO_SERIAL
#define SRL Serial1
#else
#define SRL Serial
#endif

#define EXCEPTION 'x'

void setup() {
    //SRL.begin(57600);
    SRL.begin(115200);
    //Serial.begin(38400);  // works equally well with 8MHz and 16MHz
#if ARDUINO >= 100
    while (!SRL); // for Arduino Leonardo
#endif  
    SRL.flush();
}

void loop() {
    if (SRL.available()) {
        int b = SRL.read();
        if ('a' <= b && 'z' >= b) {
            // reply by converting a lowercase character to uppercase
            SRL.write(b - 32);
            SRL.flush();
        } else {
            SRL.write(EXCEPTION);
        }
    }
}

