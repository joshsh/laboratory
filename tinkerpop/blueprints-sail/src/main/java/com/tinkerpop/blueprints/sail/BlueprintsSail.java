package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.TransactionalGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * An RDF storage interface for any Blueprints- (that is, IndexableGraph-) enabled graph database.  It models
 * RDF graphs as property graphs, using a configurable set of indices to boost query reactivity.  The graphs created
 * by this Sail can just as easily serve fast graph traversal applications as they can a SPARQL endpoint.
 * <p/>
 * RDF resources are stored as vertices, RDF statements as edges using the Blueprints default (automatic) indices.
 * Namespaces are stored at a special vertex with the id "urn:com.tinkerpop.blueprints.sail:namespaces".
 * <p/>
 * This Sail is as transactional as the underlying graph database: if the provided Graph implements TransactionalGraph
 * and is in manual transaction mode, then the SailConnection's commit and rollback methods will be used correspondingly.
 * <p/>
 * Edge indexing functions as follows.  By default, edge indices for "p", "c" and "pc" triple patterns are created.
 * That is to say that a getStatements or removeStatements call in which only the statement predicate and/or context will default to an
 * edge lookup based on the predicate and/or context values.  By default, a getStatements call with a concrete subject
 * or object will result in a vertex lookup for subject and object; adjacent edges will then be used to complete the
 * query.  Any other triple patterns which are supplied to the BlueprintsSail constructor will be treated as special;
 * an additional property will be added to each edge and used in fast lookups for queries corresponding to that pattern.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BlueprintsSail implements Sail {
    public static final String SEPARATOR = " ";

    // FIXME: temporary
    public static final String
            SUBJECT_PROP = "s",
            PREDICATE_PROP = "p",
            OBJECT_PROP = "o",
            CONTEXT_PROP = "c";

    public static final char
            URI_PREFIX = 'U',
            BLANK_NODE_PREFIX = 'B',
            PLAIN_LITERAL_PREFIX = 'P',
            TYPED_LITERAL_PREFIX = 'T',
            LANGUAGE_TAG_LITERAL_PREFIX = 'L',
            NULL_CONTEXT_PREFIX = 'N';

    public static final Pattern INDEX_PATTERN = Pattern.compile("s?p?o?c?");

    private static final String[][] ALTERNATIVES = {
            {"s", ""},
            {"p", ""},
            {"o", ""},
            {"c", ""},
            {"sp", "s", "p"},
            {"so", "s", "o"},
            {"sc", "s", "c"},
            {"po", "o", "p"},
            {"pc", "p", "c"},
            {"oc", "o", "c"},
            {"spo", "so", "sp", "po"},
            {"spc", "sc", "sp", "pc"},
            {"soc", "so", "sc", "oc"},
            {"poc", "po", "oc", "pc"},
            {"spoc", "spo", "soc", "spc", "poc"},
    };

    private static final String NAMESPACES_VERTEX_ID = "urn:com.tinkerpop.blueprints.sail:namespaces";

    private final DataStore dataStore = new DataStore();

    private final Index<Edge> edges;

    /**
     * Create a new RDF store using the provided Blueprints graph.  Default edge indices will be used.
     *
     * @param graph the storage layer.  If the provided graph implements TransactionalGraph and is in manual transaction
     *              mode, the this Sail will also be transactional.
     */
    public BlueprintsSail(final IndexableGraph graph) {
        this(graph, "p,c,pc");
        //this(graph, "s,p,o,c,sp,so,sc,po,pc,oc,spo,spc,soc,poc,spoc");
    }

    /**
     * Create a new RDF store using the provided Blueprints graph.  Additionally, create edge indices for the provided
     * triple patterns (potentially speeding up certain queries, while increasing storage overhead).
     *
     * @param graph           the storage layer.  If the provided graph implements TransactionalGraph and is in manual transaction
     *                        mode, the this Sail will also be transactional.
     * @param indexedPatterns any subset of s,p,o,c,sp,so,sc,po,pc,oc,spo,spc,soc,poc,spoc.  Only p,c are required,
     *                        while the default patterns are p,c,pc.
     */
    public BlueprintsSail(final IndexableGraph graph,
                          final String indexedPatterns) {
        dataStore.graph = graph;

        // For now, use the default EDGES and VERTICES indices, which *must exist* in Blueprints and are automatically indexed.
        // Think harder about collision issues (if someone hands a non-empty, non-Sail graph to this constructor) later on.
        edges = graph.getIndex(Index.EDGES, Edge.class);

        dataStore.namespaces = graph.getVertex(NAMESPACES_VERTEX_ID);
        if (null == dataStore.namespaces) {
            dataStore.namespaces = graph.addVertex(NAMESPACES_VERTEX_ID);
        }

        dataStore.matchers[0] = new TrivialMatcher(graph);

        parseTripleIndices(indexedPatterns);
        assignUnassignedTriplePatterns();

        dataStore.manualTransactions = dataStore.graph instanceof TransactionalGraph
                && TransactionalGraph.Mode.MANUAL == ((TransactionalGraph) dataStore.graph).getTransactionMode();

        //for (int i = 0; i < 16; i++) {
        //    System.out.println("matcher " + i + ": " + indexes.matchers[i]);
        //}
    }

    public void setDataDir(final File file) {
        throw new UnsupportedOperationException();
    }

    public File getDataDir() {
        throw new UnsupportedOperationException();
    }

    public void initialize() throws SailException {
        // Do nothing.
    }

    public void shutDown() throws SailException {
        // Do nothing.
    }

    public boolean isWritable() throws SailException {
        // For now, we assume the store is writable.
        return true;
    }

    public SailConnection getConnection() throws SailException {


        return new BlueprintsSailConnection(dataStore);
    }

    public ValueFactory getValueFactory() {
        return dataStore.valueFactory;
    }

    ////////////////////////////////////////////////////////////////////////////

    /**
     * A context object which is shared between the Blueprints Sail and its connections.
     */
    public class DataStore {
        public IndexableGraph graph;

        // We don't need a special ValueFactory implementation.
        public final ValueFactory valueFactory = new ValueFactoryImpl();

        public final Collection<IndexingMatcher> indexers = new LinkedList<IndexingMatcher>();

        // A triple pattern matcher for each spoc combination
        public final Matcher[] matchers = new Matcher[16];

        public boolean manualTransactions;

        public Vertex namespaces;
    }

    private void parseTripleIndices(final String tripleIndexes) {
        if (null == tripleIndexes) {
            throw new IllegalArgumentException("index list, if supplied, must be non-null");
        }

        Set<String> u = new HashSet<String>();

        String[] a = tripleIndexes.split(",");
        if (0 == a.length) {
            throw new IllegalArgumentException("index list, if supplied, must be non-empty");
        }
        for (String s : a) {
            u.add(s.trim());
        }

        // These two patterns are required for efficient operation.
        u.add("p");
        u.add("c");

        for (String s : u) {
            createIndexingMatcher(s);
        }
    }

    private void assignUnassignedTriplePatterns() {
        // As a first pass, fill in all suitable patterns (those containing
        // subject and/or object) not already assigned to indexing matchers,
        // with graph-based matchers.
        for (int i = 0; i < 16; i++) {
            if (null == dataStore.matchers[i]
                    && ((0 != (i & 0x1)) || (0 != (i & 0x4)))) {
                dataStore.matchers[i] = new GraphBasedMatcher(
                        (0 != (i & 0x1)),
                        (0 != (i & 0x2)),
                        (0 != (i & 0x4)),
                        (0 != (i & 0x8)),
                        dataStore.graph);
            }
        }

        // Now fill in any remaining patterns with alternative indexing matchers.
        Matcher[] n = new Matcher[16];
        n[0] = dataStore.matchers[0];
        for (String[] alts : ALTERNATIVES) {
            String p = alts[0];
            int i = indexFor(p);

            Matcher m = dataStore.matchers[i];

            // if no matcher has been assigned for this pattern
            if (null == m) {
                // try primary alternatives in the order they are specified
                for (int j = 1; j < alts.length; j++) {
                    m = dataStore.matchers[indexFor(alts[j])];
                    if (null != m) {
                        break;
                    }
                }

                // if no primary alternatives are assigned, choose the first secondary alternative
                if (null == m) {
                    m = n[1];
                }
            }

            n[i] = m;
        }

        System.arraycopy(n, 0, dataStore.matchers, 0, 16);
    }

    private int indexFor(final boolean s,
                         final boolean p,
                         final boolean o,
                         final boolean c) {
        int index = 0;

        if (s) {
            index |= 0x1;
        }
        if (p) {
            index |= 0x2;
        }
        if (o) {
            index |= 0x4;
        }
        if (c) {
            index |= 0x8;
        }

        return index;
    }

    private int indexFor(final String pattern) {
        boolean s = false, p = false, o = false, c = false;
        for (byte ch : pattern.getBytes()) {
            switch (ch) {
                case 's':
                    s = true;
                    break;
                case 'p':
                    p = true;
                    break;
                case 'o':
                    o = true;
                    break;
                case 'c':
                    c = true;
                    break;
                default:
                    throw new IllegalStateException();
            }
        }

        return indexFor(s, p, o, c);
    }

    private void createIndexingMatcher(final String pattern) {
        boolean s = false, p = false, o = false, c = false;
        for (byte ch : pattern.getBytes()) {
            switch (ch) {
                case 's':
                    s = true;
                    break;
                case 'p':
                    p = true;
                    break;
                case 'o':
                    o = true;
                    break;
                case 'c':
                    c = true;
                    break;
                default:
                    throw new IllegalStateException();
            }
        }

        int index = indexFor(s, p, o, c);
        IndexingMatcher m = new IndexingMatcher(s, p, o, c, edges);
        dataStore.matchers[index] = m;
        dataStore.indexers.add(m);
    }

    public static void debugEdge(final Edge e) {
        System.out.println("edge " + e + ":");
        for (String key : e.getPropertyKeys()) {
            System.out.println("\t" + key + ":\t'" + e.getProperty(key) + "'");
        }
        System.out.println("\t[in vertex]: " + e.getInVertex());
        System.out.println("\t[out vertex]: " + e.getOutVertex());
    }

    public static void debugVertex(final Vertex v) {
        System.out.println("vertex " + v + ":");
        for (String key : v.getPropertyKeys()) {
            System.out.println("\t" + key + ":\t'" + v.getProperty(key) + "'");
        }
        Iterator<Edge> i;
        i = v.getInEdges().iterator();
        System.out.println("\t[in edges]:");
        while (i.hasNext()) {
            System.out.println("\t\t" + i.next());
        }
        i = v.getOutEdges().iterator();
        System.out.println("\t[out edges]:");
        while (i.hasNext()) {
            System.out.println("\t\t" + i.next());
        }
    }
}
