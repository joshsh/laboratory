//#define LEONARDO_SERIAL
#define BLUETOOTH

#define CONN_PIN    A1
#define RESET_PIN   A2

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
    
#ifdef BLUETOOTH
    pinMode(CONN_PIN,  INPUT);
    pinMode(RESET_PIN, OUTPUT);
    digitalWrite(RESET_PIN, LOW);
    SRL.print("ST,0\r");
    // Expected response is "AOK"
    char buf[4];
    if (read_line(SRL, buf, sizeof(buf), 200) != rl_status::OK ||
            std::strcmp(buf, "AOK") != 0) {
        return false;
    }
        assert_reset();
    // Datasheet says a 160us pulse, but that seems inadequate.
    delay(10);
    clear_reset();

    // "Set" commands only take effect after reset
    reset();
#endif
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

