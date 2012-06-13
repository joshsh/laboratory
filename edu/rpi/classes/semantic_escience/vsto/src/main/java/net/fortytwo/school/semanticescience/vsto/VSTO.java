package net.fortytwo.school.semanticescience.vsto;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Sep 13, 2008
 * Time: 4:02:49 PM
 * To change this template use File | Settings | File Templates.
 */
public interface VSTO {
    static final URI NS = new URIImpl("http://dataportal.ucar.edu/schemas/vsto.owl#"),
            HASDESCRIPTION = new URIImpl(NS + "hasDescription"),
            INSTRUMENT = new URIImpl(NS + "Instrument"),
            MAGNETOMETER = new URIImpl(NS + "Magnetometer"),
            GROUNDBASEDINSTRUMENT = new URIImpl(NS + "GroundBasedInstrument"),
            RADAR = new URIImpl(NS + "Radar"),
            OPTICALINSTRUMENT = new URIImpl(NS + "OpticalInstrument"),
            SOUNDER = new URIImpl(NS + "Sounder"),
            REFERENCE = new URIImpl(NS + "reference");
}
