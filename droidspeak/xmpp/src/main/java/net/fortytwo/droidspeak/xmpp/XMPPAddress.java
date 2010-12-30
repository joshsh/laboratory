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
 * XMPPAddress.java
 *
 * @author Miguel Escriva (mescriva@dsic.upv.es)
 * @author Javier Palanca (jpalanca@dsic.upv.es)
 * @author Computer Technology Group (http://www.dsic.upv.es/users/ia/ia.html)
 * @version 0.1
 */


package net.fortytwo.droidspeak.xmpp;

import jade.mtp.TransportAddress;

public class XMPPAddress implements TransportAddress {

    private String _address;

    public XMPPAddress(String address) {
        if (address.regionMatches(0, "xmpp://", 0, 7) == false) {
            ; //TODO lanzar excepcion.
        }
        _address = address.substring(7, address.length());
    }

    /* (non-Javadoc)
      * @see jade.mtp.TransportAddress#getProto()
      */
    public String getProto() {
        return "xmpp";
    }

    /* (non-Javadoc)
      * @see jade.mtp.TransportAddress#getHost()
      */
    public String getHost() {
        return _address;
    }

    /* (non-Javadoc)
      * @see jade.mtp.TransportAddress#getPort()
      */
    public String getPort() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
      * @see jade.mtp.TransportAddress#getFile()
      */
    public String getFile() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
      * @see jade.mtp.TransportAddress#getAnchor()
      */
    public String getAnchor() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getJID() {
        return _address;
    }

    public String toString() {
        return new String("xmpp://" + _address);
    }

}
