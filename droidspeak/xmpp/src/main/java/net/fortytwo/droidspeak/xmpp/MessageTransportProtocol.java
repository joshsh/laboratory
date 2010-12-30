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
 * MessageTransportProtocol.java
 *
 * @author Miguel Escriva (mescriva@dsic.upv.es)
 * @author Javier Palanca (jpalanca@dsic.upv.es)
 * @author Computer Technology Group (http://www.dsic.upv.es/users/ia/ia.html)
 * @version 0.1
 */

package jade.mtp.xmpp;

import jade.util.leap.List;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.provider.ProviderManager;

import jade.core.Profile;
import jade.core.ProfileException;
import jade.core.Specifier;
import jade.domain.FIPAAgentManagement.Envelope;
import jade.mtp.*;


public class MessageTransportProtocol implements MTP {

	private static final String PREFIX = "jade_mtp_xmpp_";
	
	private String[] PROTOCOLS = { "xmpp" };

	private String FIPA_NAME = "fipa.mts.mtp.xmpp";

	public XMPPConnection connection;	
	

	private void login(String servername, String username, String passwd) throws MTPException
	{
		try{
			connection = new XMPPConnection(servername);
			connection.login(username, passwd, "acc");
			
			Presence p = new Presence(Presence.Type.AVAILABLE);
			connection.sendPacket(p);
		}
		catch (XMPPException e){
			throw new MTPException(	
			"Cannot login to server (" + e.getMessage() + ")");
		}
	}

	private void logout()
	{
		connection.close();
	}
	
	
	
	/**
	 Converts a string representing a valid address in this MTP to a
	 <code>TransportAddress</code> object.
	 @param rep The string representation of the address.
	 @return A <code>TransportAddress</code> object, created from the
	 given string.
	 @exception MTPException If the given string is not a valid
	 address according to this MTP.
	 */
	public TransportAddress strToAddr(String arg0) throws MTPException {
		return new XMPPAddress(arg0);
	}
 
	/**
	 Converts a <code>TransportAddress</code> object into a string
	 representation.
	 @param ta The <code>TransportAddress</code> object.
	 @return A string representing the given address.
	 @exception MTPException If the given
	 <code>TransportAddress</code> is not a valid address for this
	 MTP.
	 */
	public String addrToStr(TransportAddress ta) throws MTPException {
		return ((XMPPAddress) ta).toString();
	}

	/**
	 Reads the name of the message transport protocol managed by this
	 MTP. The FIPA standard message transport protocols have a name
	 starting with <code><b>"fipa.mts.mtp"</b></code>.
	 @return A string, that is the name of this MTP.
	 */
	public String getName() {
		return FIPA_NAME;
	}

	public String[] getSupportedProtocols() {
		return PROTOCOLS;
	}

	/**
	 Activates an MTP handler for incoming messages on a default
	 address.
	 @parameter p is the Profile from which the configuration parameters
	 for this instance of JADE container can be retrieved
	 @return A <code>TransportAddress</code>, corresponding to the
	 chosen default address.
	 @exception MTPException Thrown if some MTP initialization error
	 occurs.
	 */
	public TransportAddress activate(Dispatcher disp, Profile p)
			throws MTPException {
		
		String server = p.getParameter(PREFIX + "server", null); 
		String username = p.getParameter(PREFIX + "username", null); 
		String passwd = p.getParameter(PREFIX + "passwd", null); 
		
		login(server, username, passwd);
		ProviderManager.addExtensionProvider(FipaEnvelopePacketExtension.ELEMENT_NAME, FipaEnvelopePacketExtension.NAMESPACE, new FipaEnvelopePacketExtensionProvider());
		MessageListener list = new MessageListener(connection, disp);
		list.start();
		
		
		return strToAddr("xmpp://" + username + "@" + server +"/acc");
	}

	/**
	 Activates an MTP handler for incoming messages on a specific
	 address.
	 @param ta A <code>TransportAddress</code> object, representing
	 the transport address to listen to.
	 @parameter p is the Profile from which the configuration parameters
	 for this instance of JADE container can be retrieved
	 @exception MTPException Thrown if some MTP initialization error
	 occurs.
	 */
	public void activate(Dispatcher disp, TransportAddress ta, Profile p)
			throws MTPException {
		// Ignore the User suplied TransportAddress
		System.err.println("Warning!! Ignoring the User suplied TransportAddress.");
		activate(disp, p);
	}


	/**
	 Deactivates the MTP handler listening at a given transport
	 address.
	 @param ta The <code>TransportAddress</code> object the handle to
	 close is listening to.
	 @exception MTPException Thrown if some MTP cleanup error occurs.
	 */
	public void deactivate(TransportAddress ta) throws MTPException {
		deactivate();	
	}

	/**
	 Deactivates all the MTP handlers.
	 @exception MTPException Thrown if some MTP cleanup error occurs.
	 */
	public void deactivate() throws MTPException {
		logout();
	}

	/**
	 Delivers to the specified address an ACL message, encoded in some
	 concrete message representation, using the given envelope as a
	 transmission header.
	 @param ta The transport address to deliver the message to. It
	 must be a valid address for this MTP.
	 @param env The message envelope, containing various fields
	 related to message recipients, encoding, and timestamping.
	 @payload The byte sequence that contains the encoded ACL message.
	 @exception MTPException Thrown if some MTP delivery error occurs.
	 */
	public void deliver(String addr, Envelope env, byte[] payload)
			throws MTPException {
		Message msg = new Message();
		FipaEnvelopePacketExtension ext = new FipaEnvelopePacketExtension();
		ext.setEnvelope(XMLCodec.encodeXML(env));
		
		XMPPAddress jid = new XMPPAddress(addr);
		msg.setTo(jid.getJID());
		msg.setBody(new String(payload));
		msg.setType(Message.Type.NORMAL);
		msg.addExtension(ext);
		connection.sendPacket(msg);
//		System.out.println("Send: " + msg.toXML());
//		System.out.println("Deliver to: " + jid.getJID());
//		System.out.println("Deliver body: " + body);		
	}

}
