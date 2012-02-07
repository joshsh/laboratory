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

import java.net.InetAddress;
import java.text.DecimalFormat;

import com.alien.enterpriseRFID.notify.Message;
import com.alien.enterpriseRFID.notify.MessageListener;
import com.alien.enterpriseRFID.notify.MessageListenerService;
import com.alien.enterpriseRFID.reader.AlienClass1Reader;
import com.alien.enterpriseRFID.tags.Tag;
import com.alien.enterpriseRFID.tags.TagTable;
import com.alien.enterpriseRFID.tags.TagTableListener;
import com.alien.enterpriseRFID.tags.TagUtil;

/**
 * Uses the new Intelligent Tag Radar functionality to track a tag's distance
 * from an antenna, by integrating streamed speed data over time. When a tag is
 * first read, it's distance is set to zero, and subsequent speed measurements
 * are integrated over time to update the distance.
 * <p>
 * The TagStream events are delivered to the messageReceived method, where they
 * are integrated with persistent tag data stored in a TagTable. When an updated
 * measurement is received for an existing Tag, the Tag.updateSpeed() method is
 * used to perform the calculations, and the resulting distance and smoothed
 * speed data is printed.
 * <p>
 * This example also nicely demonstrates how to decode custom-formatted stream
 * data and notifications.
 * <p>
 * Only enterprise class readers (ALR-x800/9900/9650) support ITR, and
 * they must have a firmware revision of at least 08.06.26.
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
public class ITRSpeedRangeTest implements MessageListener, TagTableListener  {
  
  // Keep track of the state of the tags by using a TagTable
  private TagTable tagTable;
  
  // Use this formatter to make speed/distance output easier to read
  private DecimalFormat DF2 = new DecimalFormat("+##0.00;-##0.00");

  
/**
 * Constructor.
 */
public ITRSpeedRangeTest() throws Exception {
  tagTable = new TagTable();
  tagTable.setTagTableListener(this);
  
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
  
  // Only deal with one antenna - we could theoretically get independent speed
  // data from each antenna and analyze it separately.
  reader.setAntennaSequence("0");
  
  // Very fast reads work best, and it only works with Gen2 tags.
  reader.setTagType(AlienClass1Reader.CLASS1GEN2);
  reader.setAcquireG2Cycles(1);
  reader.setAcquireG2Count(1);

  // Set up TagStream.
  // Use this host's IPAddress, and the port number that the service is listening on.
  // getHostAddress() may find a wrong (wireless) Ethernet interface, so you may
  // need to substitute your computers IP address manually.
  reader.setTagStreamAddress(InetAddress.getLocalHost().getHostAddress(), service.getListenerPort());
  // Need to use custom format to get speed.
  // We need at least the EPC, read time in milliseconds, and the speed
  String customFormatStr = "Tag:${TAGID}, Last:${MSEC2}, Speed:${SPEED}";
  reader.setTagStreamFormat(AlienClass1Reader.CUSTOM_FORMAT);
  reader.setTagStreamCustomFormat(customFormatStr);

  // Tell the static TagUtil class about the custom format, so it can decode the streamed data.
  TagUtil.setCustomFormatString(customFormatStr);

  // Tell the MessageListenerService that the data has a custom format.
  service.setIsCustomTagList(true);
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
 * Implements the MessageListener interface.
 * 
 * Data from the reader is captured by a MessageListenerService, which then
 * tells its MessageListeners via this method.
 */
public void messageReceived(Message message){
  for (int i=0; i < message.getTagCount(); i++) {
    Tag tag = message.getTag(i);

    // TagTable will automatically merge new information about an existing
    // tag, including calculating a smoothed speed and distance update.
    tagTable.addTag(tag);
    
    // After this merge is done, the TagTable will notify us with the final
    // data via the TagTableListener interface (tagAdded, tagRenewed, etc.).
  }
}


/**
 * Implements the TagTableListener interface.
 * 
 * When a TagTable is updated, it tells its TagTableListener via these methods.
 * We let the TagTable tell us about new tag reads, so that we get access to the
 * smoothed speed and distance values without having to manually look up in the
 * TagTable after   
 */
public void tagAdded(Tag tag) {
  System.out.println("New Tag: " + tag.getTagID() + ", v0=" + DF2.format(tag.getSpeed()) + ", d0=" + DF2.format(tag.getSmoothPosition()));
}
public void tagRenewed(Tag tag) {
  System.out.println(tag.getTagID() + ", v=" + DF2.format(tag.getSmoothSpeed()) + ", d=" + DF2.format(tag.getSmoothPosition()));
}
public void tagRemoved(Tag tag) {
  // Don't care
}


/**
 * Main
 */
public static final void main(String args[]){
  try {
    new ITRSpeedRangeTest();
  } catch (Exception e) {
    System.out.println("Error:" + e.toString());
  }
}

}