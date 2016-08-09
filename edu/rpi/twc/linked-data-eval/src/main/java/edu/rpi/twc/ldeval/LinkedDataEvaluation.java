package edu.rpi.twc.ldeval;

import net.fortytwo.linkeddata.LinkedDataCache;
import net.fortytwo.linkeddata.Rdfizer;
import net.fortytwo.linkeddata.dereferencers.HTTPRepresentation;
import net.fortytwo.linkeddata.rdfizers.VerbatimRdfizer;
import net.fortytwo.ripple.RippleException;
import org.apache.commons.httpclient.HttpMethod;
import org.openrdf.model.Statement;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParser;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;
import org.restlet.data.MediaType;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class LinkedDataEvaluation {

    private static String acceptHeader;

    private static String lastHost;
    private static long lastFinished;

    private static Map<RDFFormat, Rdfizer> rdfizers;

    private static void dereferenceUriWithLogging(final String uri,
                                                  final boolean doParse) throws Exception {
        long before = System.currentTimeMillis();

        long med = before - 1;
        long after = before - 1;
        long idleTime = -1;

        HTTPRepresentation rep = null;

        String error;

        HttpMethod method;
        MediaType mt = null;

        long size = 0;

        int statusCode;
        String host;

        RDFFormat format = null;

        try {
            rep = new HTTPRepresentation(uri, acceptHeader);
            method = rep.getMethod();
            statusCode = method.getStatusCode();
            host = method.getURI().getHost();
            mt = rep.getMediaType();
            format = null == mt ? null : RDFFormat.forMIMEType(mt.getName());
            idleTime = rep.getIdleTime();

            med = System.currentTimeMillis();

            InputStream in = rep.getStream();

            try {
                if (doParse) {
                    Rdfizer r = null == format ? null : rdfizers.get(format);

                    if (null == r) {
                        size = -1;
                    } else {
                        TripleCounter c = new TripleCounter();

                        r.rdfize(in, c, "http://example.org/baseURI#");
                        size = c.getCount();
                    }
                } else {
                    while (-1 != in.read()) {
                        size++;
                    }
                }
            } finally {
                in.close();
            }

            after = System.currentTimeMillis();

            error = "none";
        } catch (HTTPRepresentation.ErrorResponseException e) {
            error = e.getClass().getSimpleName();
            method = e.getMethod();
            statusCode = method.getStatusCode();
            host = method.getHostConfiguration().getHost();

        } catch (HTTPRepresentation.InvalidResponseException e) {
            error = e.getClass().getSimpleName();
            method = e.getMethod();
            statusCode = method.getStatusCode();
            host = method.getHostConfiguration().getHost();
        } catch (Exception e) {
            // TODO: uncomment and add stack trace (or turn into a logger message)
            //System.err.println("error: " + e);
            //e.printStackTrace(System.err);
            error = e instanceof RippleException
                    ? e.getCause().getClass().getSimpleName()
                    : e.getClass().getSimpleName();
            statusCode = 0;
            host = "unknown";
        }

        System.out.print(statusCode);
        System.out.print("," + size);
        System.out.print("," + (med - before) + "," + (after - before) + "," + idleTime);
        System.out.print("," + (null == format ? "none" : format.getName()));
        System.out.print("," + host);
        System.out.println("," + error);
    }

    private static class TripleCounter implements RDFHandler {
        private long count = 0;

        public long getCount() {
            return count;
        }

        public void startRDF() throws RDFHandlerException {
        }

        public void endRDF() throws RDFHandlerException {
        }

        public void handleNamespace(String s, String s1) throws RDFHandlerException {
        }

        public void handleStatement(Statement statement) throws RDFHandlerException {
            count++;
        }

        public void handleComment(String s) throws RDFHandlerException {
        }
    }

    public static void main(final String[] args) throws Exception {
        boolean doParse = false;

        rdfizers = new HashMap<RDFFormat, Rdfizer>();
        for (RDFFormat f : RDFFormat.values()) {
            Rdfizer r = new VerbatimRdfizer(f, RDFParser.DatatypeHandling.IGNORE);
            rdfizers.put(f, r);
        }

        System.out.println("code,size,respond,transmit,idle,format,host,error");
        Sail sail = new MemoryStore();
        sail.initialize();
        try {
            LinkedDataCache cache = new LinkedDataCache(sail);
            acceptHeader = cache.getAcceptHeader();

            File file = new File("/tmp/test-uris.txt");
            BufferedReader br = new BufferedReader(new FileReader(file));
            String line;
            while ((line = br.readLine()) != null) {
                dereferenceUriWithLogging(line.trim(), doParse);
            }
            br.close();

        } finally {
            sail.shutDown();
        }
    }
}
