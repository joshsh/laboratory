package net.fortytwo.droidspeak.xmpp;

import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;

/**
 * User: josh
 * Date: 12/30/10
 * Time: 7:10 PM
 */
public class XmppPlay {
    public static void main(final String[] args) {
        try {
            new XmppPlay().doit();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }

    private void doit() throws Exception {

    }

    private class ChatAgent {
        private final XMPPConnection conn;

        private final String name;

        public ChatAgent(String name, String password, String resource) throws XMPPException {
            this.name = name;

            ConnectionConfiguration config = new ConnectionConfiguration("fortytwo.net", 5222);
            conn = new XMPPConnection(config);
            conn.connect();
            conn.login(name, password, resource);

             conn.addPacketListener(new PacketListener(){
                 public void processPacket(final Packet packet) {
                    System.out.println("processing packet: " + prettyPrint(packet));
                 }
             }, new PacketFilter(){
                 public boolean accept(final Packet packet) {
                     //System.out.println("accepting packet: " + prettyPrint(packet));
                     return true;
                 }
             });
        }

        public void disconnect() {
            conn.disconnect();
        }

        public XMPPConnection getConn() {
            return conn;
        }

        public void sendMessageTo(ChatAgent other, final String msg) {
            Message m = new Message();
            m.setBody(msg);
            m.setTo(other.name + "@fortytwo.net");

            conn.sendPacket(m);
        }

        public String getName() {
            return name;
        }
    }

    public static String prettyPrint(final Packet packet) {
        StringBuilder sb = new StringBuilder("packet ").append(packet.getPacketID()).append(":\n");
        sb.append("\tfrom: ").append(packet.getFrom()).append("\n");
        sb.append("\tto: " ).append(packet.getTo()).append("\n");
        for (String s : packet.getPropertyNames()) {
            sb.append("\tproperty '").append(s).append("': ").append(packet.getProperty(s)).append("\n");
        }
        if (packet instanceof Message) {
            String body = ((Message) packet).getBody();
            String subject = ((Message) packet).getSubject();
            String thread = ((Message) packet).getThread();
            if (null != thread) {
                        sb.append("\tthread: ").append(thread).append("\n");
            }
            if (null != subject) {
                sb.append("\tsubject: ").append(subject).append("\n");
            }
            if (null != body) {
                sb.append("\tbody: ").append(body).append("\n");
            }
        }
        return sb.toString();
    }
}
