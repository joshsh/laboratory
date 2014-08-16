
#define MODE_INPUT  0
#define MODE_OUTPUT 1

int outputPings = 100;

void setup() {
    SLIPSerial.begin(115200);
    //SLIPSerial.begin(38400);  // works equally well with 8MHz and 16MHz
#if ARDUINO >= 100
    while (!Serial); // for Arduino Leonardo
#endif  
}

int state = MODE_INPUT;

void error() {
    Serial.print('x');
}

void loop() {
    switch (state) {
        case MODE_INPUT:
            while (Serial.available()) {
                int b = Serial.read();
                if ('i' == b) {
                    Serial.print('o');
                } else if ('n' == b) {
                    state = MODE_OUTPUT;
                    break;
                } else {
                    error();
                    break;
                }  
            }
            break;
        case MODE_OUTPUT:
            int32_t startTime = millis();
            for (int i = 0; i < outputPings; i++) {
                Serial.print('i');
                while (!Serial.available());
                int b = Serial.read();
                if ('o' == b) {
                    
                } else {
                    error();
                    break;
                }  
            }
            int32_t endTime = millis();
            Serial.print(outputPings);
            Serial.print('\t');
            Serial.print(endTime - startTime);
            Serial.print('\n');
            state = MODE_INPUT;
            break;
    }
}
