package net.fortytwo.school.semanticescience.vsto;

import org.jdom.Element;
import org.jdom.Namespace;
import org.jdom.output.XMLOutputter;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.memory.MemoryStore;

import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Sep 13, 2008
 * Time: 1:27:56 PM
 * To change this template use File | Settings | File Templates.
 */
public class VSTOGeoTagger {
    private static final String BASE_URI = "http://example.org/ns/";
    public static final Namespace NS_KML = Namespace.getNamespace("http://www.opengis.net/kml/2.2");

    private Map<URI, OWLClass> uriToClass = new HashMap<URI, OWLClass>();

    private boolean inherits(final URI child,
                             final URI ancestor) {
        OWLClass childClass = getOWLClass(child);
        OWLClass ancestorClass = getOWLClass(ancestor);

        return childClass.inherits(ancestorClass);
    }

    private OWLClass getOWLClass(final URI uri) {
        OWLClass c = uriToClass.get(uri);

        if (null == c) {
            c = new OWLClass();
            c.uri = uri;
            uriToClass.put(uri, c);
        }

        return c;
    }

    private void addSubclassRelationship(final URI child,
                                         final URI parent) {
//System.out.println("" + child + " --> " + parent);
        OWLClass childClass = getOWLClass(child);
        OWLClass parentClass = getOWLClass(parent);

        childClass.superClasses.add(parentClass);
    }

    private void createInheritanceTree(final Repository repo) throws Exception {
        boolean includeInferred = false;
        RepositoryConnection rc = repo.getConnection();
        try {
            Collection<Statement> results = rc.getStatements(null, RDFS.SUBCLASSOF, null, includeInferred).asList();
            for (Statement st : results) {
                Resource subject = st.getSubject();
                Value object = st.getObject();

                if (subject instanceof URI && object instanceof URI) {
                    addSubclassRelationship((URI) subject, (URI) object);
                }
            }
        } finally {
            rc.close();
        }
    }

    private class Placemark {
        private final double latitude;
        private final double longitude;
        private final double altitude;
        private final String name;
        private final String description;
        private PaddleStyle paddleStyle = PaddleStyle.WHITE;

        public Placemark(final Resource r,
                         final Repository repo) throws Exception {
            Collection<Statement> results;
            boolean includeInferred = false;

            RepositoryConnection rc = repo.getConnection();
            try {
                results = rc.getStatements(r, WGS84.LAT, null, includeInferred).asList();
                if (0 == results.size()) {
                    throw new Exception("missing wgs84:lat statement for " + r);
                }
                latitude = Double.parseDouble(((Literal) results.iterator().next().getObject()).getLabel());

                results = rc.getStatements(r, WGS84.LONG, null, includeInferred).asList();
                if (0 == results.size()) {
                    throw new Exception("missing wgs84:long statement for " + r);
                }
                longitude = Double.parseDouble(((Literal) results.iterator().next().getObject()).getLabel());

                results = rc.getStatements(r, WGS84.ALT, null, includeInferred).asList();
                if (0 < results.size()) {
                    altitude = Double.parseDouble(((Literal) results.iterator().next().getObject()).getLabel());
                } else {
                    altitude = Double.NaN;
                }

                results = rc.getStatements(r, VSTO.HASDESCRIPTION, null, includeInferred).asList();
                if (0 < results.size()) {
                    name = ((Literal) results.iterator().next().getObject()).getLabel();
                } else {
                    name = "";
                }

                StringBuilder descBuilder = new StringBuilder();

                results = rc.getStatements(r, RDF.TYPE, null, includeInferred).asList();
                if (0 < results.size()) {
                    String types = "class: ";
                    boolean first = true;
                    for (Statement st : results) {
                        Value t = st.getObject();
                        if (t instanceof URI) {
                            paddleStyle = choosePaddleStyle((URI) t);
                            String typeName = ((URI) t).getLocalName();

                            Integer i = instanceCount.get(typeName);
                            if (null == i) {
                                i = 0;
                            }
                            instanceCount.put(typeName, i + 1);

                            if (first) {
                                first = false;
                            } else {
                                types += ", ";
                            }

                            types += typeName;
                        }
                    }

                    descBuilder.append(types);
                }

                results = rc.getStatements(r, RDFS.COMMENT, null, includeInferred).asList();
                if (0 < results.size()) {
                    if (0 < descBuilder.length()) {
                        descBuilder.append("\n\n");
                    }

                    String comment = ((Literal) results.iterator().next().getObject()).getLabel();
                    descBuilder.append(truncate(comment, 500));
//                    descBuilder.append("<div height=\"100px\">" + comment + "</div>");
//                    descBuilder.append("<font size=\"-1\">" + comment + "</font>");
                }

                results = rc.getStatements(r, VSTO.REFERENCE, null, includeInferred).asList();
                if (0 < results.size()) {
                    String url = ((Literal) results.iterator().next().getObject()).getLabel();
                    url = url.replaceAll("[:]8081", "");
                    
                    if (0 < descBuilder.length()) {
                        descBuilder.append("\n\n");
                    }

                    descBuilder.append("<a href=\"" + url + "\">more info</a>");
                }

                description = htmlize(descBuilder.toString());
            } finally {
                rc.close();
            }
        }

        private String htmlize(final String s) {
            return s.replaceAll("\n", "<br/>\n");
        }

        public Element toElement() {
            Element placemarkEl = new Element("Placemark", NS_KML);

            Element nameEl = new Element("name", NS_KML);
            nameEl.addContent(name);
            placemarkEl.addContent(nameEl);

            Element descriptionEl = new Element("description", NS_KML);
            descriptionEl.addContent(description);
            placemarkEl.addContent(descriptionEl);

            Element styleUrlEl = new Element("styleUrl", NS_KML);
            styleUrlEl.addContent("#" + paddleStyle.getStyleMapID());
            placemarkEl.addContent(styleUrlEl);

            Element pointEl = new Element("Point", NS_KML);
            Element coordinatesEl = new Element("coordinates", NS_KML);
            String c = "" + longitude + "," + latitude;
            if (!new Double(altitude).isNaN()) {
                c += "," + altitude;
            }
            coordinatesEl.addContent(c);
            pointEl.addContent(coordinatesEl);
            placemarkEl.addContent(pointEl);

            return placemarkEl;
        }
    }

    public static String truncate(final String text,
                                  final int maxLength) {
        if (maxLength >= text.length()) {
            return text;
        } else {
            String s = text.substring(0, maxLength - 3);
            int j = (32 <= text.charAt(maxLength - 3))
                    ? maxLength - 3
                    : -1;
            for (int i = s.length() - 1; 0 <= i; i--) {
                int c = s.charAt(i);
                if (32 <= c) {
                    j = i;
                } else {
                    if (-1 < j) {
                        break;
                    }
                }
            }

            if (0 < j) {
                s = s.substring(0, j);
            }

            return s + "...";
        }
    }

    private Map<String, Integer> instanceCount = new HashMap<String, Integer>();

    /*
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
 <Placemark>
   <name>Simple placemark</name>
   <description>Attached to the ground. Intelligently places itself
      at the height of the underlying terrain.</description>
   <Point>
     <coordinates>-122.0822035425683,37.42228990140251,0</coordinates>
   </Point>
 </Placemark>
</kml>
    */

    private Collection<Placemark> extractPlacemarks(final Repository repo) throws Exception {
        Collection<Statement> results;
        boolean includeInferred = false;

        Collection<Resource> spatialThings = new HashSet<Resource>();
        RepositoryConnection rc = repo.getConnection();
        try {
            results = rc.getStatements(null, WGS84.LAT, null, includeInferred).asList();
            for (Statement st : results) {
                spatialThings.add(st.getSubject());
            }
        } finally {
            rc.close();
        }

        Collection<Placemark> placemarks = new LinkedList<Placemark>();
        for (Resource r : spatialThings) {
            placemarks.add(new Placemark(r, repo));
        }

        return placemarks;
    }

    private Element createKML(final Collection<Placemark> placemarks) throws Exception {
        Element kml = new Element("kml", NS_KML);
        Element document = new Element("document", NS_KML);
        kml.addContent(document);

        document.addContent(PaddleStyle.RED.styleElement());
        document.addContent(PaddleStyle.YELLOW.styleElement());
        document.addContent(PaddleStyle.GREEN.styleElement());
        document.addContent(PaddleStyle.BLUE.styleElement());
        document.addContent(PaddleStyle.WHITE.styleElement());

        document.addContent(PaddleStyle.RED.styleMapElement());
        document.addContent(PaddleStyle.YELLOW.styleMapElement());
        document.addContent(PaddleStyle.GREEN.styleMapElement());
        document.addContent(PaddleStyle.BLUE.styleMapElement());
        document.addContent(PaddleStyle.WHITE.styleMapElement());

        Element name = new Element("name", NS_KML);
        name.addContent("VSTO instruments");
        document.addContent(name);
        Element description = new Element("description", NS_KML);
        description.addContent("instruments in the VSTO ontology with known geo coordinates");
        document.addContent(description);
        Element open = new Element("open", NS_KML);
        open.addContent("1");
        document.addContent(open);

        Element folder = new Element("folder", NS_KML);
        document.addContent(folder);
        name = new Element("name", NS_KML);
        name.addContent("[folder] VSTO instruments");
        folder.addContent(name);
        description = new Element("description", NS_KML);
        description.addContent("[folder] instruments in the VSTO ontology with known geo coordinates");
        folder.addContent(description);
        open = new Element("open", NS_KML);
        open.addContent("1");
        folder.addContent(open);

        for (Placemark p : placemarks) {
            folder.addContent(p.toElement());
        }

        return kml;
    }

    public Element createKML() throws Exception {
        Repository repo = new SailRepository(new MemoryStore());
        repo.initialize();

        Element el;

        try {
            addOntologyData(repo);
            createInheritanceTree(repo);

            Collection<Placemark> placemarks = extractPlacemarks(repo);
            el = createKML(placemarks);
        } finally {
            repo.shutDown();
        }

//        for (String s : instanceCount.keySet()) {
//            System.out.println("class: " + s + "(" + instanceCount.get(s) + ")");
//        }

        return el;
    }

    private PaddleStyle choosePaddleStyle(final URI classURI) {
        if (inherits(classURI, VSTO.RADAR)) {
            return PaddleStyle.RED;
        } else if (inherits(classURI, VSTO.OPTICALINSTRUMENT)) {
            return PaddleStyle.BLUE;
        } else {
            return PaddleStyle.WHITE;
        }
    }

    private void addOntologyData(final Repository repo) throws Exception {

        String[] files = {
                "cedar.owl",
                "csac.owl",
                "mlso.owl",
                "vsto.owl",
                "vsto_all.owl",
                "VSTO_ext.owl",
                "wgs84.owl"
        };

        RepositoryConnection rc = repo.getConnection();
        try {
            for (String file : files) {
                InputStream is = VSTOGeoTagger.class.getResourceAsStream(file);
                try {
//                    System.out.println("importing data from file " + file);
                    rc.add(is, BASE_URI, RDFFormat.RDFXML);
                } finally {
                    is.close();
                }
            }
        } finally {
            rc.close();
        }
    }

    public static final void main(final String[] args) throws Exception {
        VSTOGeoTagger t = new VSTOGeoTagger();
        Element el = t.createKML();

//        System.out.println(el.toString());
        XMLOutputter out = new XMLOutputter();
        out.output(el, System.out);
    }
}
