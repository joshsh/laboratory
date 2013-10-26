package com.franz.benchmarking.sp2bench;

import com.franz.benchmarking.GraphFactory;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SP2BenchLoader {
    private static final long BUFFER_SIZE = 1000;

    private static final long LOGGING_BUFFER_SIZE = 10000;

    private static final String BASE_URI = "http://example.org/baseURI/";

    private static long count;
    private static boolean verbose = true;

    public static void main(final String[] args) throws Exception {
        final Map<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("http://localhost/vocabulary/bench/", "bench");
        namespaces.put("http://purl.org/dc/elements/1.1/", "dc");
        namespaces.put("http://purl.org/dc/terms/", "dcterms");
        namespaces.put("http://xmlns.com/foaf/0.1/", "foaf");
        namespaces.put("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
        namespaces.put("http://www.w3.org/2000/01/rdf-schema#", "rdfs");
        namespaces.put("http://swrc.ontoware.org/ontology#", "swrc");

        final Neo4jGraph g = GraphFactory.createNeo4jGraph("/tmp/neo-sp2bench");
        final IdGraph ig = new IdGraph(g, true, false);

        //InputStream in = new FileInputStream(new File("/Users/josh/data/shortterm/franz/sp2bench/sp2b-5e4.nt"));
        InputStream in = new FileInputStream(new File("/tmp/sp2b-5e4.nt"));
        try {
            RDFHandler h = new RDFHandler() {
                public void startRDF() throws RDFHandlerException {
                }

                public void endRDF() throws RDFHandlerException {
                }

                public void handleNamespace(String s, String s2) throws RDFHandlerException {
                }

                public void handleStatement(final Statement s) throws RDFHandlerException {
                    URI p = s.getPredicate();
                    String label = uriToString(p, namespaces);

                    Vertex subject = vertexForValue(s.getSubject(), ig, namespaces);
                    Vertex object = vertexForValue(s.getObject(), ig, namespaces);

                    ig.addEdge(null, subject, object, label);

                    incrementCount(g);
                }

                public void handleComment(String s) throws RDFHandlerException {
                }
            };

            RDFParser p = Rio.createParser(RDFFormat.NTRIPLES);
            p.setRDFHandler(h);

            count = 0;
            long before = System.currentTimeMillis();
            p.parse(in, "http://example.org/baseURI/");
            g.commit();
            long after = System.currentTimeMillis();

            System.out.println("imported " + count + " statements in " + (after - before) + "ms");
        } finally {
            in.close();
        }

        g.shutdown();
    }

    private static Vertex vertexForValue(final Value v,
                                         final Graph g,
                                         final Map<String, String> namespaces) {
        if (v instanceof URI) {
            String s = uriToString((URI) v, namespaces);
            Vertex n = g.getVertex(s);
            return null == n ? g.addVertex(s) : n;
        } else if (v instanceof BNode) {
            Vertex n = g.getVertex("_:" + ((BNode) v).getID());
            return null == n ? g.addVertex("_:" + ((BNode) v).getID()) : n;
        } else if (v instanceof Literal) {
            // TODO: verify that the queries allow us to ignore the datatype of literals
            Vertex n = g.getVertex("\"" + ((Literal) v).getLabel() + "\"");
            return null == n ? g.addVertex("\"" + ((Literal) v).getLabel() + "\"") : n;
        } else {
            throw new IllegalStateException("value is of unknown type: " + v);
        }
    }

    private static String uriToString(final URI u,
                                      final Map<String, String> namespaces) {

        String ns = u.getNamespace();
        if (namespaces.keySet().contains(ns)) {
            String prefix = namespaces.get(ns);
            return prefix + ":" + u.getLocalName();
        } else {
            return u.stringValue();
        }
    }

    private static void incrementCount(final TransactionalGraph g) {
        count++;
        if (0 == count % BUFFER_SIZE) {
            g.commit();

            if (verbose && 0 == count % LOGGING_BUFFER_SIZE) {
                System.out.println("" + System.currentTimeMillis() + "\t" + count);
            }
        }
    }
}
