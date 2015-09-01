package net.fortytwo;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import org.openrdf.model.Statement;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class WordnetLoader {
    private static final Logger logger = Logger.getLogger(WordnetLoader.class.getName());

    private static final String SOURCE = "/Users/josh/data/shortterm/wordnet-loading/wordnet/rdf";
    private static final String DEST = "/Users/josh/data/shortterm/wordnet-loading/neo4j";
    private static final long BUFFER_SIZE = 1000;

    public static void main(final String[] args) {
        try {
            new WordnetLoader().load();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private void load() throws Exception {
        Neo4jGraph g = new Neo4jGraph(DEST);

        Sail sail = new GraphSail(g);
        sail.initialize();
        try {
            SailConnection c = sail.getConnection();
            try {
                c.begin();

                long startTime = System.currentTimeMillis();

                File dir = new File(SOURCE);
                for (File f : dir.listFiles()) {
                    long before = System.currentTimeMillis();
                    logger.info("loading file: " + f);
                    String n = f.getName();
                    InputStream is = null;
                    if (n.endsWith(".gz")||n.endsWith(".gzip")) {
                        is = new GZIPInputStream(new FileInputStream(f));
                        n = n.substring(0, n.lastIndexOf('.'));
                    }

                    RDFFormat format = RDFFormat.forFileName(n);
                    if (null == format) {
                        continue;
                    }

                    if (null == is) {
                        is = new FileInputStream(f);
                    }

                    RDFParser p = Rio.createParser(format);
                    p.setStopAtFirstError(false);
                    p.setRDFHandler(new SailConnectionAdder(c));

                    try {
                        p.parse(is, "http://example.org/wordnet/");
                    } catch (Throwable t) {
                        // Attempt to recover.
                        t.printStackTrace(System.err);
                    } finally {
                        is.close();
                    }

                    long after = System.currentTimeMillis();
                    logger.info("\tfinished in " + (after - before) + "ms");
                }

                long endTime = System.currentTimeMillis();
                logger.info("resulting triple store has " + c.size() + " statements");
                logger.info("total load time: " + (endTime - startTime) + "ms");
            } finally {
                c.rollback();
                c.close();
            }
        } finally {
            sail.shutDown();
        }
    }

    private class SailConnectionAdder implements RDFHandler {
        private final SailConnection c;
        private long count = 0;

        private SailConnectionAdder(SailConnection c) throws SailException {
            this.c = c;
        }

        public void startRDF() throws RDFHandlerException {
        }

        public void endRDF() throws RDFHandlerException {
            try {
                c.commit();
                c.begin();
            } catch (SailException e) {
                throw new RDFHandlerException(e);
            }
        }

        public void handleNamespace(String prefix, String uri) throws RDFHandlerException {
            try {
                c.setNamespace(prefix, uri);
            } catch (SailException e) {
                throw new RDFHandlerException(e);
            }

            incrementCount();
        }

        public void handleStatement(Statement statement) throws RDFHandlerException {
            try {
                c.addStatement(statement.getSubject(), statement.getPredicate(), statement.getObject(), statement.getContext());
            } catch (SailException e) {
                throw new RDFHandlerException(e);
            }

            incrementCount();
        }

        public void handleComment(String s) throws RDFHandlerException {
        }

        private void incrementCount() throws RDFHandlerException {
            count++;
            if (0 == count % BUFFER_SIZE) {
                try {
                    c.commit();
                    c.begin();
                } catch (SailException e) {
                    throw new RDFHandlerException(e);
                }
            }
        }
    }
}
