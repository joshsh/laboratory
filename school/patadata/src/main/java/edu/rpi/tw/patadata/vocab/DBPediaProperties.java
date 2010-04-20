package edu.rpi.tw.patadata.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * User: josh
 * Date: Apr 16, 2010
 * Time: 7:04:58 PM
 */
public interface DBPediaProperties {
    public static final String NAMESPACE = "http://dbpedia.org/property/";

    public static final URI WIKILINK = new URIImpl(NAMESPACE + "wikilink");
}
