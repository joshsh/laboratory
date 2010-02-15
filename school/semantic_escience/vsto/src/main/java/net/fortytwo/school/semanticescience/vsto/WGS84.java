package net.fortytwo.school.semanticescience.vsto;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Sep 13, 2008
 * Time: 3:43:55 PM
 * To change this template use File | Settings | File Templates.
 */
public interface WGS84 {
    static final URI NS = new URIImpl("http://fortytwo.net/2008/08/wgs84.owl#"),
            ALT = new URIImpl(NS + "alt"),
            LAT = new URIImpl(NS + "lat"),
            LONG = new URIImpl(NS + "long"),
            POINT = new URIImpl(NS + "Point"),
            SPATIALTHING = new URIImpl(NS + "SpatialThing");
}
