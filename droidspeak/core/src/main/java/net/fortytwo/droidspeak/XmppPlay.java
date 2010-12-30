package net.fortytwo.droidspeak;

import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.XMPPConnection;

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
        ConnectionConfiguration config = new ConnectionConfiguration("fortytwo.net", 5222);
        XMPPConnection c = new XMPPConnection(config);
        c.connect();



        c.disconnect();
    }
}
