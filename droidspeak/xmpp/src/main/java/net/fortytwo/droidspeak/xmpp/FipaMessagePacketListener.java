/*****************************************************************
 JADE - Java Agent DEvelopment Framework is a framework to develop
 multi-agent systems in compliance with the FIPA specifications.
 Copyright (C) 2000 CSELT S.p.A.

 The updating of this file to JADE 2.0 has been partially supported by the
 IST-1999-10211 LEAP Project

 This file refers to parts of the FIPA 99/00 Agent Message Transport
 Implementation Copyright (C) 2000, Laboratoire d'Intelligence
 Artificielle, Ecole Polytechnique Federale de Lausanne

 GNU Lesser General Public License

 This library is free software; you can redistribute it sand/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation,
 version 2.1 of the License.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the
 Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 Boston, MA  02111-1307, USA.
 *****************************************************************/

/**
 * MessageListener.java
 *
 * @author Miguel Escriva (mescriva@dsic.upv.es)
 * @author Javier Palanca (jpalanca@dsic.upv.es)
 * @author Computer Technology Group (http://www.dsic.upv.es/users/ia/ia.html)
 * @version 0.1
 */


package net.fortytwo.droidspeak.xmpp;

import java.io.StringReader;

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.filter.MessageTypeFilter;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.PacketExtension;

import jade.domain.FIPAAgentManagement.Envelope;
import jade.mtp.MTPException;
import jade.mtp.InChannel.Dispatcher;

public class FipaMessagePacketListener implements PacketListener {
    private final XMPPConnection connection;
    private final Dispatcher dispatcher;

    public FipaMessagePacketListener(XMPPConnection connection, Dispatcher dispatcher) {
        this.connection = connection;
        this.dispatcher = dispatcher;
    }

    public void finalize() {
        connection.removePacketListener(this);
    }

    public void start() {
        PacketFilter filter = new MessageTypeFilter(Message.Type.normal);
        connection.addPacketListener(this, filter);
    }

    public void processPacket(Packet packet) {
        System.out.println("processing packet: " + XmppPlay.prettyPrint(packet));

        Envelope env;
        Message msg = (Message) packet;
        String payload = msg.getBody();

        try {
            XMLCodec parser = new XMLCodec("org.apache.xerces.parsers.SAXParser");
            PacketExtension ext = msg.getExtension(FipaEnvelopePacketExtension.ELEMENT_NAME, FipaEnvelopePacketExtension.NAMESPACE);
            if (ext == null) {
                throw new MTPException("Message does not contain an Envelope!");
            }
            FipaEnvelopePacketExtension fipaext = (FipaEnvelopePacketExtension) ext;

            StringReader sr = new StringReader(fipaext.getEnvelope());
            env = parser.parse(sr);
            synchronized (dispatcher) {
                dispatcher.dispatchMessage(env, payload.getBytes());
				System.out.println("mensage enviado!");
            }
        } catch (MTPException e) {
            e.printStackTrace(System.err);
        }
    }

}
