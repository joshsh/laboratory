package edu.rpi.twc.sesamestream.util;

import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.impl.ValueFactoryBase;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ErrorTolerantValueFactory extends ValueFactoryBase {
    private static final URI GOOD_URI = new URIImpl("http://example.org/substitute-for-bad-uri");

    private final ValueFactory base;

    public ErrorTolerantValueFactory(ValueFactory base) {
        this.base = base;
    }

    @Override
    public URI createURI(String s) {
        try {
            return base.createURI(s);
        } catch (IllegalArgumentException e) {
            return GOOD_URI;
        }
    }

    @Override
    public URI createURI(String s, String s1) {
        try {
            return base.createURI(s, s1);
        } catch (IllegalArgumentException e) {
            return GOOD_URI;
        }
    }

    @Override
    public BNode createBNode(String s) {
        return base.createBNode(s);
    }

    @Override
    public Literal createLiteral(String s) {
        return base.createLiteral(s);
    }

    @Override
    public Literal createLiteral(String s, String s1) {
        return base.createLiteral(s, s1);
    }

    @Override
    public Literal createLiteral(String s, URI uri) {
        return base.createLiteral(s, uri);
    }

    @Override
    public Statement createStatement(Resource resource, URI uri, Value value) {
        return base.createStatement(resource, uri, value);
    }

    @Override
    public Statement createStatement(Resource resource, URI uri, Value value, Resource resource1) {
        return base.createStatement(resource, uri, value, resource1);
    }
}
