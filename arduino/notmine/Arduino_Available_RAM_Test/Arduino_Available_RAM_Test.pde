// This is some example code for exploring memory allocation in Arduino
//   written by Rob Faludi http://www.faludi.com
//   more information can be found online:
//     http://ccrma.stanford.edu/courses/250a/lectures/microcontrollers/
//     http://www.nongnu.org/avr-libc/user-manual/malloc.html
//     http://en.wArduino_Available_RAM_TestArduino_Available_RAM_TestArduino_Available_RAM_Testikipedia.org/wiki/Malloc


void setup() {
  ////// set up and blink the status LED once////////
  pinMode(13,OUTPUT); // set up the status LED
  digitalWrite(13, HIGH); // turn on the status LED
  delay(20); // wait so the blink is visitlbe
  digitalWrite(13, LOW); // turn off the status LED
  //////////////////////////////////////////////////

  Serial.begin(9600); // start serial for debugging output

  // run the memory test function and print the results to the serial port
  int result = memoryTest();
  Serial.print("Memory test results: ");
  Serial.print(result,DEC);
  Serial.print(" bytes free");
}



void loop () {
; // nothing happens in the loop
}



// this function will return the number of bytes currently free in RAM
int memoryTest() {
  int byteCounter = 0; // initialize a counter
  byte *byteArray; // create a pointer to a byte array
  // More on pointers here: http://en.wikipedia.org/wiki/Pointer#C_pointers

  // use the malloc function to repeatedly attempt allocating a certain number of bytes to memory
  // More on malloc here: http://en.wikipedia.org/wiki/Malloc
  while ( (byteArray = (byte*) malloc (byteCounter * sizeof(byte))) != NULL ) {
    byteCounter++; // if allocation was successful, then up the count for the next try
    free(byteArray); // free memory after allocating it
  }
  
  free(byteArray); // also free memory after the function finishes
  return byteCounter; // send back the highest number of bytes successfully allocated
}


