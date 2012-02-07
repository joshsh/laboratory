/**
 * Copyright 2008 Alien Technology Corporation. All rights reserved.
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
import com.alien.enterpriseRFID.tags.*;
import com.alien.enterpriseRFID.notify.*;

import java.net.InetAddress;

/**
 * Starts up a message listener service, then opens a connection to a reader
 * connected to COM1 and configures it to go into autonomous mode and stream tag
 * reads back to this application.
 * <p>
 * The TagStream events are delivered to the messageReceived method, where the
 * tag list is then displayed.
 * <p>
 * Only enterprise class readers (ALR-x800/9900/9650) support TagStreaming, and
 * they must have a firmware revision of at least 07.01.31.
 * 
 * One thing to note: This application will run for 10 seconds, and then it will
 * reconnect to the reader and turn off AutoMode and TagStreamMode. If you don't
 * exit this application nicely, say with a ctrl-C or similar method, the reader
 * is still reading and streaming tags, even though the application has exited.
 * <p>
 * The solution to this is to log onto the reader and turn AutoMode off.
 *
 * @version 1.0 July 2008
 * @author David Krull
 */
public class TagStreamTest implements MessageListener  {

/**
 * Constructor.
 */
public TagStreamTest() throws Exception {
  // Set up the message listener service.
  // It handles streamed data as well as notifications.
  MessageListenerService service = new MessageListenerService(4000);
  service.setMessageListener(this);
  service.startService();
  System.out.println("Message Listener has Started");

  // Instantiate a new reader object, and open a connection to it on COM1
  AlienClass1Reader reader = new AlienClass1Reader("COM1");
  reader.open();
  System.out.println("Configuring Reader");

  // Set up TagStream.
  // Use this host's IPAddress, and the port number that the service is listening on.
  // getHostAddress() may find a wrong (wireless) Ethernet interface, so you may
  // need to substitute your computers IP address manually.
  reader.setTagStreamAddress(InetAddress.getLocalHost().getHostAddress(), service.getListenerPort());
  reader.setTagStreamFormat(AlienClass1Reader.TEXT_FORMAT); // Make sure service can decode it.
  reader.setTagStreamMode(AlienClass1Reader.ON);

  // Set up AutoMode - use standard settings.
  reader.autoModeReset();
  reader.setAutoMode(AlienClass1Reader.ON);

  // Close the connection and spin while messages arrive
  reader.close();
  long runTime = 10000; // milliseconds
  long startTime = System.currentTimeMillis();
  do {
    Thread.sleep(1000);
  } while(service.isRunning() && (System.currentTimeMillis()-startTime) < runTime);
  
  // Reconnect to the reader and turn off AutoMode and TagStreamMode.
  System.out.println("\nResetting Reader");
  reader.open();
  reader.autoModeReset();
  reader.setTagStreamMode(AlienClass1Reader.OFF);
  reader.close();
}


/**
 * A single Message has been received from a Reader.
 *
 * @param message the notification message received from the reader
 */
public void messageReceived(Message message){
  System.out.println("\nStream Data Received:");
  if (message.getTagCount() == 0) {
    System.out.println("(No Tags)");
  } else {
    for (int i = 0; i < message.getTagCount(); i++) {
      Tag tag = message.getTag(i);
      System.out.println(tag.toLongString());
    }
  }
}


/**
 * Main
 */
public static final void main(String args[]){
  try {
    new TagStreamTest();
  } catch (Exception e) {
    System.out.println("Error:" + e.toString());
  }
}

} // End of class TagStreamTest