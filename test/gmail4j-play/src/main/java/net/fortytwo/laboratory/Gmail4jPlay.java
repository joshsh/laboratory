package net.fortytwo.laboratory;

import com.googlecode.gmail4j.EmailAddress;
import com.googlecode.gmail4j.GmailClient;
import com.googlecode.gmail4j.GmailConnection;
import com.googlecode.gmail4j.GmailException;
import com.googlecode.gmail4j.GmailMessage;
import com.googlecode.gmail4j.auth.Credentials;
import com.googlecode.gmail4j.javamail.ImapGmailClient;
import com.googlecode.gmail4j.javamail.ImapGmailConnection;
import org.apache.commons.lang3.StringEscapeUtils;

import java.io.File;
import java.io.FileInputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

public class Gmail4jPlay {
    private static final String
            GMAIL_USERNAME = "gmail.username",
            GMAIL_PASSWORD = "gmail.password",
            GMAIL_ADDRESSES = "gmail.addresses";

    public static void main(final String[] args) throws Exception {
        Properties config = new Properties();
        config.load(new FileInputStream(new File("/tmp/gmail.props")));
        String userName = config.getProperty(GMAIL_USERNAME);
        String password = config.getProperty(GMAIL_PASSWORD);
        String tmp = config.getProperty(GMAIL_ADDRESSES);

        if (null == userName || null == password) {
            throw new Exception("missing user name or password");
        }

        Set<String> addresses = new HashSet<String>();
        for (String a : tmp.split(",")) {
            addresses.add(a.trim());
        }

        //*
        Credentials login = new Credentials();
        login.setUsername(userName);
        login.setPassword(password.toCharArray());
        //*/

        //Credentials login = new LoginDialog().show("Gmail Login");

        GmailConnection conn = new ImapGmailConnection();
        conn.setLoginCredentials(login);

        //configure connection
        GmailClient client = new ImapGmailClient();
        client.setConnection(conn);
        List<GmailMessage> unreadMessages = client.getUnreadMessages();

        for (GmailMessage m : unreadMessages) {
            try {
                if (isFromMe(m, addresses) || isToMe(m, addresses)) {
                    log(m);
                    /*
                    System.out.println("message:");
                    //System.out.println("\tlink: " + m.getLink());
                    //System.out.println("\tcontext text: " + m.getContentText());
                    //System.out.println("\tcc: " + m.getCc());
                    System.out.println("\tfrom: " + m.getFrom());
                    System.out.println("\theader info: " + m.getMessageHeaderInfo());
                    System.out.println("\tmessage number: " + m.getMessageNumber());
                    System.out.println("\tsend date: " + m.getSendDate());
                    System.out.println("\tto:");
                    for (EmailAddress a : m.getTo()) {
                        System.out.println("\t\t" + a);
                    }
                    */
                } else {
                    //  System.out.println("to: " + m.getTo());
                }
            } catch (GmailException e) {
                if (null != e.getCause() && e.getCause() instanceof NullPointerException) {
                    System.err.println("null pointer exception: " + e.getMessage());
                } else {
                    throw e;
                }
            }
        }
    }

    private static void log(GmailMessage m) {
        for (EmailAddress a : m.getTo()) {
            System.out.println("" + m.getSendDate().getTime()
                    + "," + csvEscape(m.getFrom().getEmail())
                    + "," + (null == m.getFrom().getName() ? "null" : csvEscape(m.getFrom().getName()))
                    + "," + (null == a.getName() ? "null" : csvEscape(a.getEmail()))
                    + "," + csvEscape(a.getName()));
        }
    }

    private static String csvEscape(final String s) {
        return StringEscapeUtils.escapeCsv(s);
    }

    private static boolean isFromMe(final GmailMessage m,
                                    final Set<String> myAddresses) {
        return myAddresses.contains(m.getFrom().getEmail());
    }

    private static boolean isToMe(final GmailMessage m,
                                  final Set<String> myAddresses) {
        for (EmailAddress a : m.getTo()) {
            if (isMe(a, myAddresses)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isMe(final EmailAddress a,
                                final Set<String> myAddresses) {
        return myAddresses.contains(a.getEmail());
    }
}
