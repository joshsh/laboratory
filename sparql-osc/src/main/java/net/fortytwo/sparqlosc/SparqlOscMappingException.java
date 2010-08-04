package net.fortytwo.sparqlosc;

/**
 * An exception which is raised when SPARQL query results conflict with the SPARQL-to-OSC mapping defined for a query.
 * 
 * User: josh
 * Date: Jul 31, 2010
 * Time: 6:01:33 PM
 */
public class SparqlOscMappingException extends Exception {
    public SparqlOscMappingException(final String msg) {
        super(msg);
    }

    public SparqlOscMappingException(final String msg,
                                     final Throwable cause) {
        super(msg, cause);
    }
}
