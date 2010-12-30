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
 * FipaEnvelopePacketExtensionProvider.java
 *
 * @author Miguel Escriva (mescriva@dsic.upv.es)
 * @author Javier Palanca (jpalanca@dsic.upv.es)
 * @author Computer Technology Group (http://www.dsic.upv.es/users/ia/ia.html)
 * @version 0.1
 */
 
package jade.mtp.xmpp;

import org.jivesoftware.smack.packet.PacketExtension;
import org.jivesoftware.smack.provider.PacketExtensionProvider;
import org.xmlpull.v1.XmlPullParser;


public class FipaEnvelopePacketExtensionProvider implements
		PacketExtensionProvider {

	public FipaEnvelopePacketExtensionProvider() {
	}

	public PacketExtension parseExtension(XmlPullParser parser) throws Exception {

		boolean done = false;
		StringBuffer buffer = new StringBuffer();
		String contentype;
		
		// TODO: Leer el content-type
		FipaEnvelopePacketExtension FipaExtension = new FipaEnvelopePacketExtension();

		while(!done){
			int eventType = parser.next();
			if (eventType == XmlPullParser.START_TAG) {
				buffer.append(parser.getText());
			}
			else if (eventType == XmlPullParser.TEXT) {
				buffer.append(parser.getText());				
			}
			if (eventType == XmlPullParser.END_TAG) {
				if (parser.getName().equals(FipaExtension.getElementName())) {
					done = true;
				}
				else{
					buffer.append(parser.getText());
				}
			}
		}
		
		FipaExtension.setEnvelope(buffer.toString());
		return FipaExtension;
	}

}
