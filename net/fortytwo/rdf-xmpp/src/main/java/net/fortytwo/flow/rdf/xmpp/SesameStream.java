package net.fortytwo.flow.rdf.xmpp;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * User: josh
 * Date: Mar 29, 2010
 * Time: 7:04:07 PM
 */
public class SesameStream {
    public static final String
            NAMESPACE = "http://fortytwo.net/2010/04/sesamestream#";

    public static final String
            XMPP_SERVER = "net.fortytwo.flow.rdf.xmpp.server",
            XMPP_PORT = "net.fortytwo.flow.rdf.xmpp.port",
            XMPP_REPORTER_USERNAME = "net.fortytwo.flow.rdf.xmpp.reporterUsername",
            XMPP_REPORTER_PASSWORD = "net.fortytwo.flow.rdf.xmpp.reporterPassword",
            XMPP_REASONER_USERNAME = "net.fortytwo.flow.rdf.xmpp.reasonerUsername",
            XMPP_REASONER_PASSWORD = "net.fortytwo.flow.rdf.xmpp.reasonerPassword";

    private static final Properties CONFIGURATION;
    private static final Logger LOGGER;

    static {
        CONFIGURATION = new Properties();

        try {
            CONFIGURATION.load(SesameStream.class.getResourceAsStream("testing.properties"));
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }

        InputStream resourceAsStream = SesameStream.class.getResourceAsStream("logging.properties");

        try {
            LogManager.getLogManager().readConfiguration(resourceAsStream);
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        LOGGER = getLogger(SesameStream.class);
        LOGGER.info("initialized logging");
    }

    public static Properties getConfiguration() {
        return CONFIGURATION;
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c.getName());
    }
}
