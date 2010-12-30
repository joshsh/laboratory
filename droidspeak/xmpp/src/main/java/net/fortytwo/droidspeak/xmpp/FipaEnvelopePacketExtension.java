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
 * FipaEnvelopePacketExtension.java
 *
 * @author Miguel Escriva (mescriva@dsic.upv.es)
 * @author Javier Palanca (jpalanca@dsic.upv.es)
 * @author Computer Technology Group (http://www.dsic.upv.es/users/ia/ia.html)
 * @version 0.1
 */


package net.fortytwo.droidspeak.xmpp;

import org.jivesoftware.smack.packet.PacketExtension;


public class FipaEnvelopePacketExtension implements PacketExtension {


    public static final String ELEMENT_NAME = "x";
    public static final String NAMESPACE = "jabber:x:fipa";

    private String contentType;
    private String envelope;


    public FipaEnvelopePacketExtension() {
        this.contentType = "fipa.mts.env.rep.xml.std";
    }

    public FipaEnvelopePacketExtension(String contentType) {
        this.contentType = contentType;
    }

    public String getElementName() {
        return ELEMENT_NAME;
    }

    public String getNamespace() {
        return NAMESPACE;
    }

    public String getContentType() {
        return contentType;
    }

    public void setEnvelope(String envelope) {
        this.envelope = envelope;
    }

    public String getEnvelope() {
        return envelope;
    }

    public String toXML() {
        StringBuffer buf = new StringBuffer();
        buf.append("<").append(ELEMENT_NAME).append(" xmlns=\"").append(NAMESPACE).append("\"").append(" content-type=\"").append(contentType).append("\">");
        buf.append(envelope);
        buf.append("</").append(ELEMENT_NAME).append(">");
        return buf.toString();
    }

}
