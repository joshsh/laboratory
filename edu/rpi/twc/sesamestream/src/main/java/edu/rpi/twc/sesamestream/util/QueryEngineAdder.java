package edu.rpi.twc.sesamestream.util;

import edu.rpi.twc.sesamestream.QueryEngine;
import org.openrdf.model.Statement;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class QueryEngineAdder implements RDFHandler {
    private final QueryEngine queryEngine;

    public QueryEngineAdder(QueryEngine queryEngine) {
        this.queryEngine = queryEngine;
    }

    public void startRDF() throws RDFHandlerException {
    }

    public void endRDF() throws RDFHandlerException {
    }

    public void handleNamespace(String s, String s1) throws RDFHandlerException {
    }

    public void handleStatement(Statement s) throws RDFHandlerException {
        queryEngine.addStatement(s);
    }

    public void handleComment(String s) throws RDFHandlerException {
    }
}
