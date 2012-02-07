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

import com.alien.enterpriseRFID.discovery.*;

/**
 * Starts a NetworkDiscoveryService to listen for Alien Reader
 * heartbeats that are broadcast over the local subnet. The discovery service
 * notifies this application when a reader is discovered, seen again, or lost.
 *
 * @version 1.3 Aug 2008
 * @author David Krull
 */

public class NetworkDiscoveryTest implements DiscoveryListener {

/**
 * Constructor
 */
public NetworkDiscoveryTest() throws Exception {
  NetworkDiscoveryListenerService service = new NetworkDiscoveryListenerService();
  service.setDiscoveryListener(this);
  service.startService();

  // Spin while readers are discovered.
  while (service.isRunning()) {
    Thread.sleep(100);
  }
}


/**
 * A new reader has been discovered to the network.
 * This method implements the DiscoveryListener interface.
 *
 * @param discoveryItem details of the newly-discovered reader
 */
public void readerAdded(DiscoveryItem discoveryItem){
  System.out.println("Reader Added:\n" + discoveryItem.toString());
}

/**
 * A known reader has been seen again.
 * This method implements the DiscoveryListener interface.
 *
 * @param discoveryItem details of the renewed reader
 */
public void readerRenewed(DiscoveryItem discoveryItem) {
  System.out.println("Reader Renewed:\n" + discoveryItem.toString());
}


/**
 * A reader has been removed from the network and is no longer available.
 * This method implements the DiscoveryListener interface.
 *
 * @param discoveryItem details of the removed reader
 */
public void readerRemoved(DiscoveryItem discoveryItem) {
  System.out.println("Reader Removed:\n" + discoveryItem.toString());
}


/**
 * Main
 */
public static final void main(String args[]) {
  try {
    new NetworkDiscoveryTest();
  } catch (Exception e) {
    System.out.println("Error:" + e.toString());
  }
}

}