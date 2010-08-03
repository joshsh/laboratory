package net.fortytwo.sparqlosc;

import java.net.InetAddress;
import java.net.SocketException;
import java.util.LinkedList;
import java.util.List;

/**
 * User: josh
 * Date: Jul 31, 2010
 * Time: 6:01:21 PM
 */
public class SparqlOscSubscriber {
    private final String sparqlQuery;
    private final InetAddress inetAddress;
    private final int port;
    private final String addressPattern;
    private final List<SparqlOscArgument> arguments;

    public SparqlOscSubscriber(final String sparqlQuery,
                               final InetAddress inetAddress,
                               final int port,
                               final String addressPattern) throws SocketException {
        this.sparqlQuery = sparqlQuery;
        this.inetAddress = inetAddress;
        this.port = port;
        this.addressPattern = addressPattern;
        this.arguments = new LinkedList<SparqlOscArgument>();

        if (null == sparqlQuery || null == inetAddress || null == addressPattern) {
            throw new IllegalArgumentException("null argument");
        }
    }

    public void addArgument(final String name,
                            final OscType type) {
        if (null == name || 0 == name.length() || null == type) {
            throw new IllegalArgumentException("null or empty argument");
        }

        arguments.add(new SparqlOscArgument(name, type));
    }

    public List<SparqlOscArgument> getArguments() {
        return arguments;
    }

    public String getSparqlQuery() {
        return sparqlQuery;
    }

    public InetAddress getInetAddress() {
        return inetAddress;
    }

    public int getPort() {
        return port;
    }

    public String getAddressPattern() {
        return addressPattern;
    }

    public boolean equals(final Object other) {
        if (!(other instanceof SparqlOscSubscriber)) {
            return false;
        } else {
            if (this == other) {
                return true;
            }

            SparqlOscSubscriber s = (SparqlOscSubscriber) other;
            return s.sparqlQuery.equals(sparqlQuery)
                    && s.inetAddress.equals(inetAddress)
                    && s.port == port
                    && s.addressPattern.equals(addressPattern);
        }
    }

    public int hashCode() {
        return sparqlQuery.hashCode()
                + inetAddress.hashCode()
                + port
                + addressPattern.hashCode();
    }
}
