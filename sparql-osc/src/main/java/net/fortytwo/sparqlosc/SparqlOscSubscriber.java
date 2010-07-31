package net.fortytwo.sparqlosc;

import java.net.InetAddress;
import java.net.SocketException;

/**
 * User: josh
* Date: Jul 31, 2010
* Time: 6:01:21 PM
*/
public class SparqlOscSubscriber {
    private final String sparqlQuery;
    private final String[] variableNames;
    private final InetAddress inetAddress;
    private final int port;
    private final String addressPattern;

    public SparqlOscSubscriber(final String sparqlQuery,
                               final String[] variableNames,
                               final InetAddress inetAddress,
                               final int port,
                               final String addressPattern) throws SocketException {
        this.sparqlQuery = sparqlQuery;
        this.variableNames = variableNames;
        this.inetAddress = inetAddress;
        this.port = port;
        this.addressPattern = addressPattern;
    }

    public String getSparqlQuery() {
        return sparqlQuery;
    }

    public String[] getVariableNames() {
        return variableNames;
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
}
