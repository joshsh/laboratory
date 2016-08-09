package edu.rpi.tw.patadata.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

public interface DBPediaProperties {
    public static final String NAMESPACE = "http://dbpedia.org/property/";

    public static final URI WIKILINK = new URIImpl(NAMESPACE + "wikilink");
}
