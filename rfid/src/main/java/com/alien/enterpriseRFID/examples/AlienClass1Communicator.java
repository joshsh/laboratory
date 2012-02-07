/**
 * Copyright 2006 Alien Technology Corporation. All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * <p>
 * 1)	Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * <p>
 * 2)	Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * <p>
 * 3)	Neither the name of Alien Technology Corporation nor the names of any
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL ALIEN TECHNOLOGY CORPORATION OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * <p>
 * For further information, contact :
 * <p>
 * Alien Technology
 * 18220 Butterfield Blvd.
 * Morgan Hill, CA 95037
 */

package com.alien.enterpriseRFID.examples;

import com.alien.enterpriseRFID.reader.*;
import java.io.*;

/**
 * Connects to a Reader on COM port #1 and begins an interactive session. Enter
 * "q" to quit the session.
 *
 * @version 1.1 Feb 2004
 * @author David Krull
 */
public class AlienClass1Communicator {

/**
 * Constructor
 */
public AlienClass1Communicator() throws Exception {
  AlienClass1Reader reader = new AlienClass1Reader("COM1"); // Create reader object
  reader.open(); // Open the reader connection

  // Use stdin for user input
  BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

  do {
    System.out.print("\nAlien>"); // Show prompt
    String line = in.readLine(); // Grab user input
    if (line.equals("q")) break; // Quit when "q" is pressed
    System.out.println(reader.doReaderCommand(line)); // Send command, print result
  } while (true); // Repeat indefinitely

  System.out.println("\nGoodbye.");
  reader.close(); // Close the reader connection
}


/**
 * Main
 */
public static final void main(String args[]){
  try {
    new AlienClass1Communicator();
  } catch(Exception e) {
    System.out.println("Error: " + e.toString());
  }
}

} // End of class AlienClass1Communicator