package net.fortytwo.sparqlosc;

/**
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
