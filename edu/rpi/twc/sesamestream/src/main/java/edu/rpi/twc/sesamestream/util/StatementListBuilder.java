package edu.rpi.twc.sesamestream.util;

import org.openrdf.model.Statement;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.LinkedList;
import java.util.List;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class StatementListBuilder implements RDFHandler {
    private List<Statement> list;

    public void startRDF() throws RDFHandlerException {
        list = new LinkedList<Statement>();
    }

    public void endRDF() throws RDFHandlerException {
    }

    public void handleNamespace(String s, String s1) throws RDFHandlerException {
    }

    public void handleStatement(Statement s) throws RDFHandlerException {
        list.add(s);
    }

    public void handleComment(String s) throws RDFHandlerException {
    }

    public List<Statement> getStatements() {
        return list;
    }
}
