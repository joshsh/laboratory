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

import com.alien.enterpriseRFID.discovery.DiscoveryItem;
import com.alien.enterpriseRFID.discovery.DiscoveryListener;
import com.alien.enterpriseRFID.discovery.NetworkDiscoveryListenerService;
import com.alien.enterpriseRFID.reader.AlienClass1Reader;
import com.alien.enterpriseRFID.tags.Tag;

/**
 * Starts a NetworkDiscoveryService to listen for Alien Reader
 * heartbeats that are broadcast over the local subnet. The discovery service
 * notifies this application when a reader is discovered, seen again, or lost.
 *
 * @author David Krull
 * @version 1.3 Aug 2008
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
    public void readerAdded(DiscoveryItem discoveryItem) {
        //SecurityManager sec = System.getSecurityManager();

        System.out.println("Reader Added:\n" + discoveryItem.toString());
        try {
            AlienClass1Reader r = discoveryItem.getReader();
            System.out.println("\treader: " + r);
            //System.exit(0);
            System.out.println("\tis open: " + r.isOpen());
            r.setUsername("alien");
            r.setPassword("password");

            r.open();
            System.out.println("\tgetting tag list");
            Tag[] tags = r.getTagList();
            if (null != tags) {
                for (Tag t : r.getTagList()) {
                    System.out.println("\t\ttag: " + t);
                }
            }
            System.out.println("\ttag type: " + r.getTagType());
            System.out.println("\tRF level: " + r.getRFLevel());
            System.out.println("\tRF levels:");
            for (int i = 0; i < 4; i++) {
                System.out.println("\t\tantenna " + i + ": " + r.getRFLevel(i));
            }
            System.out.println("\tverify tag: " + r.verifyTag());
            System.out.println("\ttag list antenna combie: " + r.getTagListAntennaCombine());
            System.out.println("\t: " + r);
            System.out.println("\t: " + r);
            System.out.println("\t: " + r);
            System.out.println("\t: " + r);
            System.out.println("\t: " + r);
            r.clearTagList();
            System.out.println("\tdone");
        } catch (Exception e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        
        System.out.println("nothing left to do.  Exiting");
        System.exit(0);
    }

    /**
     * A known reader has been seen again.
     * This method implements the DiscoveryListener interface.
     *
     * @param discoveryItem details of the renewed reader
     */
    public void readerRenewed(DiscoveryItem discoveryItem) {
        System.out.println("Reader Renewed:\n" + discoveryItem.toString());

        try {
            AlienClass1Reader r = discoveryItem.getReader();
            System.out.println("\treader: " + r);
            //System.exit(0);
            System.out.println("\tis open: " + r.isOpen());
            r.open();
        } catch (Exception e) {
            e.printStackTrace(System.err);
        }
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