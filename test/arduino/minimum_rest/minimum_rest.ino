/*
A sketch to investigate minimum audible rests between identical tones
produced by an Arduino.

Tiny rests, all the way down to 0ms -- where you are hearing the phase mismatch
of subsequent tones -- are perceptible, but by 10ms you have a very clear rest.
*/

#define SPEAKER_PIN  9

const int toneLength = 200;
const int restLength = 10;

void setup() {
    pinMode(SPEAKER_PIN, OUTPUT);
}

void loop() {
    tone(SPEAKER_PIN, 440);
    delay(toneLength);
    noTone(SPEAKER_PIN);
    delay(restLength);
}

