

const int pinX = A0;
const int pinY = A1;
const int pinZ = A2;

const int xmin = 170;
const int xmax = 540;

const int ymin = 210;
const int ymax = 570;

const int zmin = 110;
const int zmax = 490;

const int xrange = xmax - xmin;
const int yrange = ymax - ymin;
const int zrange = zmax - zmin;

const double xmid = (xmin + xmax) / 2.0;
const double ymid = (ymin + ymax) / 2.0;
const double zmid = (zmin + zmax) / 2.0;

void setup() {
    //pinMode(pinX, INPUT);
    //pinMode(pinY, INPUT);
    //pinMode(pinZ, INPUT);
    
    Serial.begin(115200);
}

void loop() {
    
    double dx, dy, dz;
     
    dx = 2 * (analogRead(pinX) - xmid) / xrange;
    dy = 2 * (analogRead(pinY) - ymid) / yrange;
    dz = 2 * (analogRead(pinZ) - zmid) / zrange;
    
    Serial.print(micros());
    Serial.print(","); Serial.print(dx);
    Serial.print(","); Serial.print(dy);
    Serial.print(","); Serial.print(dz);
    Serial.println("");
    
    /*
    int xraw = analogRead(pinX);
    int yraw = analogRead(pinY);
    int zraw = analogRead(pinZ);
    
    Serial.print(micros());
    Serial.print(","); Serial.print(xraw);
    Serial.print(","); Serial.print(yraw);
    Serial.print(","); Serial.print(zraw);
    Serial.println("");
    
    delay(500);
    */
}
