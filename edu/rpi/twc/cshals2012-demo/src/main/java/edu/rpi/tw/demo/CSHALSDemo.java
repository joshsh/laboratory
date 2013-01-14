package edu.rpi.tw.demo;

import net.fortytwo.linkeddata.sail.LinkedDataSail;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.sesametools.ldserver.LinkedDataServer;
import net.fortytwo.sesametools.ldserver.query.SparqlResource;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailException;
import org.openrdf.sail.nativerdf.NativeStore;
import org.restlet.resource.Directory;

import java.io.File;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class CSHALSDemo {
    private final Sail sail;

    public static void main(final String[] args) {
        try {
            CSHALSDemo demo = new CSHALSDemo();
            try {
                demo.doDemo();
            } finally {
                demo.shutDown();
            }
        } catch (Throwable t) {
            t.printStackTrace();
            System.exit(1);
        }
    }

    private CSHALSDemo() throws SailException, RippleException {
        Sail cache = new NativeStore(new File("/home/ubuntu/services/linked-data/data"));
        cache.initialize();
        sail = new LinkedDataSail(cache);
        sail.initialize();
    }

    private void doDemo() throws Exception {
        LinkedDataServer server = new LinkedDataServer(
                sail,
                "http://example.org",
                "http://fortytwo.net:8000",
                8000);

        final String staticContentDir = "/home/ubuntu/services/linked-data/SNORQL/snorql";
        /*
        Application fileApp = new Application() {
            public Restlet createInboundRoot() {
                return new Directory(getContext(), staticContentDir);
            }
        };

        server.getComponent().getDefaultHost().attach("/", fileApp);
        */

        server.getHost().attach("/", new Directory(server.getContext(), "file://" + staticContentDir + "/"));
        server.getHost().attach("/scriptaculous/", new Directory(server.getContext(), "file://" + staticContentDir + "/scriptaculous/"));
        server.getHost().attach("/sparql-endpoint", new SparqlResource());

        server.start();
    }

    private void shutDown() throws SailException {
        sail.shutDown();
    }
}
