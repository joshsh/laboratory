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
 * XMLCodec.java
 *
 *
 * @author Jose Antonio Exposito
 * @author MARISM-A Development group ( marisma-info@ccd.uab.es )
 * @version 0.1
 * @author Nicolas Lhuillier
 * @author Joan Ametller
 * @version 1.0
 * @author Miguel Escriva (mescriva@dsic.upv.es)
 * @author Javier Palanca (jpalanca@dsic.upv.es)
 * @version 1.1
 */


package net.fortytwo.droidspeak.xmpp;

import jade.util.leap.Iterator;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import jade.mtp.MTPException;
import jade.domain.FIPAAgentManagement.Envelope;
import jade.domain.FIPAAgentManagement.ReceivedObject;
import jade.domain.FIPAAgentManagement.Property;
import jade.core.AID;
import jade.util.Logger;

import org.apache.commons.codec.binary.Base64;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.io.Serializable;

public class XMLCodec extends DefaultHandler {

    // Constants
    public final static String ENVELOPE_TAG = "envelope";
    public final static String PARAMS_TAG = "params";
    public final static String INDEX = "index";
    public final static String INDEX_ATTR = " index=\"";
    public final static String TO_TAG = "to";
    public final static String AID_TAG = "agent-identifier";
    public final static String AID_NAME = "name";
    public final static String AID_ADDRESSES = "addresses";
    public final static String AID_ADDRESS = "url";
    public final static String FROM_TAG = "from";
    public final static String COMMENTS_TAG = "comments";
    public final static String REPRESENTATION_TAG = "acl-representation";
    public final static String LENGTH_TAG = "payload-length";
    public final static String ENCODING_TAG = "payload-encoding";
    public final static String DATE_TAG = "date";
    //public final static String ENCRYPTED_TAG = "encrypted";
    public final static String INTENDED_TAG = "intended-receiver";
    public final static String RECEIVED_TAG = "received";
    public final static String RECEIVED_DATE = "received-date";
    public final static String RECEIVED_BY = "received-by";
    public final static String RECEIVED_FROM = "received-from";
    public final static String RECEIVED_ID = "received-id";
    public final static String RECEIVED_VIA = "received-via";
    public final static String RECEIVED_ATTR = "value";
    public final static String PROP_TAG = "user-defined";
    public final static String PROP_ATTR = "href";
    public final static String PROP_ATTR_TYPE = "type";
    public final static String PROP_STRING_TYPE = "string";
    public final static String PROP_BYTE_TYPE = "byte-array";
    public final static String PROP_SER_TYPE = "serialized";
    public final static String OT = "<";
    public final static String ET = "</";
    public final static String CT = ">";
    public final static String NULL = "";

    private XMLReader parser = null;
    private Envelope env;
    private ReceivedObject ro = null;
    private AID aid = null;
    private Property prop = null;
    // Accumulate parsed text
    private StringBuffer accumulator;
    private String propType;
    //logging
    private static Logger logger = Logger.getMyLogger(XMLCodec.class.getName());

    //var for detected tag to then origin=0, or tag from then origin=1
    //private int origin;


    /**
     * Constructor:
     *
     * @param parserClass the SAX parser class to use
     */
    public XMLCodec(String parserClass) throws NewMtpException {
        try {
            parser = (XMLReader) Class.forName(parserClass).newInstance();
        } catch (InstantiationException e) {
            throw new NewMtpException(e);
        } catch (IllegalAccessException e) {
            throw new NewMtpException(e);
        } catch (ClassNotFoundException e) {
            throw new NewMtpException(e);
        }
        parser.setContentHandler(this);
        parser.setErrorHandler(this);
    }

    // ***************************************************
    // *               Encoding methods                  *
    // ***************************************************

    /**
     * Encode the information of Agent, Tags To and From *
     */
    private static void encodeAid(StringBuffer sb, AID aid) {
        sb.append(OT).append(AID_TAG).append(CT);
        encodeTag(sb, AID_NAME, aid.getName());
        sb.append(OT).append(AID_ADDRESSES).append(CT);

        String[] addresses = aid.getAddressesArray();
        for (int i = 0; i < addresses.length; i++) {
            encodeTag(sb, AID_ADDRESS, addresses[i]);
        }
        sb.append(ET).append(AID_ADDRESSES).append(CT);
        sb.append(ET).append(AID_TAG).append(CT);
    }

    /**
     * This does the following:
     * < tag >
     * content
     * </ tag >
     */
    private static void encodeTag(StringBuffer sb, String tag, String content) {
        sb.append(OT).append(tag).append(CT);
        sb.append(content);
        sb.append(ET).append(tag).append(CT);
    }

    /**
     * A user-defined property (String name, Object value) is encoded the following way:
     * <user-defined href="name" type="type">value</user-defined>
     */
    private static void encodeProp(StringBuffer sb, Property p) {
        String v = null;
        Object o = p.getValue();
        String type = PROP_STRING_TYPE;
        if (o instanceof String) {
            v = (String) o;
        } else if (o instanceof byte[]) {
            type = PROP_BYTE_TYPE;
            v = new String(Base64.encodeBase64((byte[]) o));
        } else if (o instanceof Serializable) {
            type = PROP_SER_TYPE;
            try {
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                ObjectOutputStream oos = new ObjectOutputStream(bos);
                oos.writeObject(o);
                oos.close();
                byte[] bytes = bos.toByteArray();
                if (bytes != null)
                    v = new String(Base64.encodeBase64(bytes));
            } catch (IOException ioe) {
                return;
            }
        } else {
            return;
        }
        sb.append(OT).append(PROP_TAG).append(" ");
        sb.append(PROP_ATTR).append("=\"").append(p.getName()).append("\" ");
        sb.append(PROP_ATTR_TYPE).append("=\"").append(type).append("\"");
        sb.append(CT);
        sb.append(v);
        sb.append(ET).append(PROP_TAG).append(CT);
    }

    private void decodeProp(StringBuffer acc, Property p) {
        if (propType.equals(PROP_SER_TYPE)) {
            try {
                ObjectInputStream ois = new ObjectInputStream(
                        new ByteArrayInputStream(Base64.decodeBase64(acc.toString().getBytes("US-ASCII"))));
                p.setValue((Serializable) ois.readObject());
            } catch (Exception e) {
                // nothing, we leave value of this property as null;
            }
        } else if (propType.equals(PROP_BYTE_TYPE)) {
            try {
                p.setValue(Base64.decodeBase64(acc.toString().getBytes("US-ASCII")));
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            p.setValue(acc.toString());
        }
        propType = null;
    }

    private static void encodeOneLineTag(StringBuffer sb, String tag1, String tag2, String value) {
        sb.append(OT).append(tag1).append(" ");
        sb.append(tag2).append("=\"").append(value).append("\"/>");
    }

    /**
     * General Encoding of the envelope
     */
    public static synchronized String encodeXML(Envelope env) {

        //Create the message XML
        StringBuffer sb = new StringBuffer();
        sb.append(OT).append(ENVELOPE_TAG).append(CT);
        sb.append(OT).append(PARAMS_TAG).append(INDEX_ATTR).append(1).append("\"").append(CT);

        //Create tag TO
        Iterator i;
        for (i = env.getAllTo(); i.hasNext();) {
            sb.append(OT).append(TO_TAG).append(CT);
            encodeAid(sb, (AID) i.next());
            sb.append(ET).append(TO_TAG).append(CT);
        }

        //Create tag from
        if (env.getFrom() != null) {
            sb.append(OT).append(FROM_TAG).append(CT);
            encodeAid(sb, env.getFrom());
            sb.append(ET).append(FROM_TAG).append(CT);
        }

        //Create tag comments
        if ((env.getComments() != null) &&
                (env.getComments().length() > 0)) {
            encodeTag(sb, COMMENTS_TAG, env.getComments());
        }

        //Create tag acl-representation
        if (env.getAclRepresentation() != null) {
            encodeTag(sb, REPRESENTATION_TAG, env.getAclRepresentation());
        }

        //Create tag payload-length
        if (env.getPayloadLength() != null) {
            //System.out.println("Length: "+env.getPayloadLength());
            encodeTag(sb, LENGTH_TAG, String.valueOf(env.getPayloadLength()));
        }

        //Create tag payload-encoding
        if ((env.getPayloadEncoding() != null) &&
                (env.getPayloadEncoding().length() > 0)) {
            encodeTag(sb, ENCODING_TAG, env.getPayloadEncoding());
        }

        //Create tag date
        //Create object BAsicFipaDateTime
        BasicFipaDateTime date = new BasicFipaDateTime(env.getDate());
        if (date != null) {
            encodeTag(sb, DATE_TAG, date.toString());
        }

        //Create tag encrypted (NL: not sure it is still in FIPA)
        /*
          for (i=env.getAllEncrypted();i.hasNext();) {
          encodeTag(sb,ENCRYPTED_TAG,i.next().toString());
          }
        */

        //Create tag intended-receiver
        for (i = env.getAllIntendedReceiver(); i.hasNext();) {
            sb.append(OT).append(INTENDED_TAG).append(CT);
            encodeAid(sb, (AID) i.next());
            sb.append(ET).append(INTENDED_TAG).append(CT);
        }

        //Create tags for user properties
        for (i = env.getAllProperties(); i.hasNext();) {
            encodeProp(sb, (Property) i.next());
        }

        //Create tag received
        ReceivedObject ro = env.getReceived();
        if (ro != null) {
            //Create tag received
            sb.append(OT).append(RECEIVED_TAG).append(CT);
            //Date
            String value = new BasicFipaDateTime(ro.getDate()).toString();
            if (value != null) {
                encodeOneLineTag(sb, RECEIVED_DATE, RECEIVED_ATTR, value);
            }
            //By
            if (((value = ro.getBy()) != null) && (!value.equals(NULL))) {
                encodeOneLineTag(sb, RECEIVED_BY, RECEIVED_ATTR, value);
            }
            //From
            if (((value = ro.getFrom()) != null) && (!value.equals(NULL))) {
                encodeOneLineTag(sb, RECEIVED_FROM, RECEIVED_ATTR, value);
            }
            //Id
            if (((value = ro.getId()) != null) && (!value.equals(NULL))) {
                encodeOneLineTag(sb, RECEIVED_ID, RECEIVED_ATTR, value);
            }
            //Via
            if (((value = ro.getVia()) != null) && (!value.equals(NULL))) {
                encodeOneLineTag(sb, RECEIVED_VIA, RECEIVED_ATTR, value);
            }
            sb.append(ET).append(RECEIVED_TAG).append(CT);
        }
        sb.append(ET).append(PARAMS_TAG).append(CT);
        sb.append(ET).append(ENVELOPE_TAG).append(CT);

        return sb.toString();
    }

    // ***************************************************
    // *               Decoding methods                  *
    // ***************************************************

    /**
     * This method is called when start the document XML
     */
    public void startDocument() {
        env = new Envelope();
    }

    /**
     * This method is called at the end of parsing
     */
    public void endDocument() {

        //Put the ro object in to envelope
        //env.setReceived(ro);
    }

    /**
     * This method is called when jmp event of begin element.
     */
    public void startElement(String uri, String localName, String rawName, Attributes attributes) {
        //Detection of the begin of to or from tags

        //Update the acumulator
        accumulator = new StringBuffer();

        if (TO_TAG.equalsIgnoreCase(localName)) {
            aid = new AID();
            env.addTo(aid);
        } else if (FROM_TAG.equalsIgnoreCase(localName)) {
            aid = new AID();
            env.setFrom(aid);
        } else if (INTENDED_TAG.equalsIgnoreCase(localName)) {
            aid = new AID();
            env.addIntendedReceiver(aid);
        } else if (RECEIVED_TAG.equalsIgnoreCase(localName)) {
            ro = new ReceivedObject();
            env.addStamp(ro);
        } else if (RECEIVED_BY.equalsIgnoreCase(localName)) {
            ro.setBy(attributes.getValue(RECEIVED_ATTR));
        } else if (RECEIVED_FROM.equalsIgnoreCase(localName)) {
            ro.setFrom(attributes.getValue(RECEIVED_ATTR));
        } else if (RECEIVED_DATE.equalsIgnoreCase(localName)) {
            ro.setDate(new BasicFipaDateTime(attributes.getValue(RECEIVED_ATTR)).getTime());
        } else if (RECEIVED_ID.equalsIgnoreCase(localName)) {
            ro.setId(attributes.getValue(RECEIVED_ATTR));
        } else if (RECEIVED_VIA.equalsIgnoreCase(localName)) {
            ro.setVia(attributes.getValue(RECEIVED_ATTR));
        } else if (PROP_TAG.equalsIgnoreCase(localName)) {
            prop = new Property();
            env.addProperties(prop);
            prop.setName(attributes.getValue(PROP_ATTR));
            propType = attributes.getValue(PROP_ATTR_TYPE);
        }
    }

    /**
     * This method is called the end of element
     */
    public void endElement(String namespaceURL, String localName, String qname) {

        //Capture the value the attributes of class
        if (AID_NAME.equalsIgnoreCase(localName)) {
            aid.setName(accumulator.toString());
        } else if (AID_ADDRESS.equalsIgnoreCase(localName)) {
            aid.addAddresses(accumulator.toString());
        } else if (COMMENTS_TAG.equalsIgnoreCase(localName)) {
            env.setComments(accumulator.toString());
        } else if (REPRESENTATION_TAG.equalsIgnoreCase(localName)) {
            env.setAclRepresentation(accumulator.toString());
        } else if (LENGTH_TAG.equalsIgnoreCase(localName)) {
            env.setPayloadLength(new Long(accumulator.toString()));
            if (logger.isLoggable(Logger.WARNING))
                logger.log(Logger.FINE, "Length: " + env.getPayloadLength());
        } else if (ENCODING_TAG.equalsIgnoreCase(localName)) {
            env.setPayloadEncoding(accumulator.toString());
        } else if (DATE_TAG.equalsIgnoreCase(localName)) {
            env.setDate(new BasicFipaDateTime(accumulator.toString()).getTime());
        } else if (PROP_TAG.equalsIgnoreCase(localName)) {
            decodeProp(accumulator, prop);
            //prop.setValue(accumulator.toString());
        }
        /*
           // Not sure it is still in FIPA
           else if (ENCRYPTED_TAG.equalsIgnoreCase(localName)) {
           env.addEncrypted(accumulator.toString());
           }
        */
    }

    /**
     * This method is called when exist characters in the elements
     */
    public void characters(char[] buffer, int start, int length) {
        accumulator.append(buffer, start, length);
    }

    /**
     * This method is called when warning occur
     */
    public void warning(SAXParseException exception) {
        if (logger.isLoggable(Logger.WARNING))
            logger.log(Logger.WARNING, " line " + exception.getLineNumber() + ": " +
                    exception.getMessage());
    }

    /**
     * This method is called when errors occur
     */
    public void error(SAXParseException exception) {
        if (logger.isLoggable(Logger.WARNING))
            logger.log(Logger.WARNING, "ERROR: line " + exception.getLineNumber() + ": " +
                    exception.getMessage());
    }

    /**
     * This method is called when non-recoverable errors occur.
     */
    public void fatalError(SAXParseException exception) throws SAXException {
        if (logger.isLoggable(Logger.WARNING))
            logger.log(Logger.SEVERE, "FATAL: line " + exception.getLineNumber() + ": " +
                    exception.getMessage());
        throw exception;
    }

    /**
     * Main method
     */
    public Envelope parse(Reader in) throws MTPException {
        try {
            parser.parse(new InputSource(in));
            return env;
        } catch (Exception ex) {
            throw new MTPException(ex.getMessage());
        }
    }

}//End of XMLCodec class
