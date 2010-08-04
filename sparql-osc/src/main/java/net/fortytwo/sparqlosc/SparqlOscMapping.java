package net.fortytwo.sparqlosc;

import java.net.InetAddress;
import java.net.SocketException;
import java.util.LinkedList;
import java.util.List;

/**
 * A well-defined mapping between a SPARQL query and an OSC message.
 * <p/>
 * User: josh
 * Date: Jul 31, 2010
 * Time: 6:01:21 PM
 */
public class SparqlOscMapping {
    private final String sparqlQuery;
    private final InetAddress inetAddress;
    private final int port;
    private final String addressPattern;
    private final List<SparqlOscVariable> variables;

    /**
     * First construct a mapping object before adding individual variables with addVariable.
     *
     * @param sparqlQuery    the SPARQL query to be evaluated
     * @param inetAddress    the address for generated messages
     * @param port           the port for generated messages
     * @param addressPattern the OSC address pattern for generated messages
     * @throws SocketException if the server cannot connect
     */
    public SparqlOscMapping(final String sparqlQuery,
                            final InetAddress inetAddress,
                            final int port,
                            final String addressPattern) throws SocketException {
        this.sparqlQuery = sparqlQuery;
        this.inetAddress = inetAddress;
        this.port = port;
        this.addressPattern = addressPattern;
        this.variables = new LinkedList<SparqlOscVariable>();

        if (null == sparqlQuery || null == inetAddress || null == addressPattern) {
            throw new IllegalArgumentException("null argument");
        }
    }

    /**
     * Adds a variable to this mapping.  Variables mediate between bound values in SPARQL results and OSC arguments.
     *
     * @param name the name of corresponding variable in the SPARQL query
     * @param type the expected data type for values bound to the SPARQL variable
     */
    public void addVariable(final String name,
                            final OscType type) {
        if (null == name || 0 == name.length() || null == type) {
            throw new IllegalArgumentException("null or empty argument");
        }

        variables.add(new SparqlOscVariable(name, type));
    }

    /**
     * @return the defined variables of this mapping
     */
    public List<SparqlOscVariable> getVariables() {
        return variables;
    }

    /**
     * @return the SPARQL query to be evaluated
     */
    public String getSparqlQuery() {
        return sparqlQuery;
    }

    /**
     * @return the network address for generated messages
     */
    public InetAddress getInetAddress() {
        return inetAddress;
    }

    /**
     * @return the port for generated messages
     */
    public int getPort() {
        return port;
    }

    /**
     * @return the OSC address pattern for generated messages
     */
    public String getAddressPattern() {
        return addressPattern;
    }

    public boolean equals(final Object other) {
        if (!(other instanceof SparqlOscMapping)) {
            return false;
        } else {
            if (this == other) {
                return true;
            }

            SparqlOscMapping s = (SparqlOscMapping) other;
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
