package com.franz.benchmarking.sp2bench;

import com.franz.benchmarking.GraphFactory;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
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

    private static final String
            DEST = "dest",
            SOURCE = "source",
            VERBOSE = "verbose";

    private static long count;

    private String source, dest;
    private boolean verbose;

    /*
    JAVA_OPTIONS="-Xms4G -Xmx4G"

    time ./load-sp2bench.sh --source /Users/josh/data/shortterm/franz/sp2bench/sp2bench-50000.nt --dest /tmp/sp2bench-neo/50k 2>&1 | tee /tmp/sp2bench-load-50k.txt
    time ./load-sp2bench.sh --source /Users/josh/data/shortterm/franz/sp2bench/sp2bench-1000000.nt --dest /tmp/sp2bench-neo/1m 2>&1 | tee /tmp/sp2bench-load-50k.txt

    # 18s on flux
    # 24s on marvin5
    time ./load-sp2bench.sh --source /home/josh/data/datasets/sp2bench/sp2bench-50000.nt --dest /tmp/sp2bench-neo/50k 2>&1 | tee /tmp/sp2bench-load-50k.txt

    # 364s on flux
    # 766s on marvin5
    time ./load-sp2bench.sh --source /home/josh/data/datasets/sp2bench/sp2bench-1000000.nt --dest /tmp/sp2bench-neo/1m 2>&1 | tee /tmp/sp2bench-load-1m.txt
     */
    public static void main(final String[] args) {
        try {
            new SP2BenchLoader().load(args);
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private void load(final String[] args) throws Exception {
        Options options = new Options();

        Option destOpt = new Option(DEST, true, "location (file or host) of database to which to load the data");
        destOpt.setRequired(false);
        options.addOption(destOpt);

        Option sourceOpt = new Option(SOURCE, true, "path to RDF import file");
        sourceOpt.setRequired(true);
        options.addOption(sourceOpt);

        Option verboseOpt = new Option(VERBOSE, false, "whether to print detailed status information");
        verboseOpt.setRequired(false);
        options.addOption(verboseOpt);

        CommandLineParser clp = new PosixParser();
        CommandLine cmd = clp.parse(options, args);

        source = cmd.getOptionValue(SOURCE);
        dest = cmd.getOptionValue(DEST);
        verbose = cmd.hasOption(VERBOSE);

        System.out.println("loading data file " + source + " into Neo4j graph at " + dest);

        final Map<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("http://localhost/vocabulary/bench/", "bench");
        namespaces.put("http://purl.org/dc/elements/1.1/", "dc");
        namespaces.put("http://purl.org/dc/terms/", "dcterms");
        namespaces.put("http://xmlns.com/foaf/0.1/", "foaf");
        namespaces.put("http://localhost/persons/", "person");
        namespaces.put("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
        namespaces.put("http://www.w3.org/2000/01/rdf-schema#", "rdfs");
        namespaces.put("http://swrc.ontoware.org/ontology#", "swrc");

        final Neo4jGraph g = GraphFactory.createNeo4jGraph(dest);
        final IdGraph ig = new IdGraph(g, true, false);

        InputStream in = new FileInputStream(new File(source));
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

    private void incrementCount(final TransactionalGraph g) {
        count++;
        if (0 == count % BUFFER_SIZE) {
            g.commit();

            if (verbose && 0 == count % LOGGING_BUFFER_SIZE) {
                System.out.println("" + System.currentTimeMillis() + "\t" + count);
            }
        }
    }

    protected static void exitWithError(final String msg) {
        System.err.println(msg);
        System.exit(1);
    }
}
