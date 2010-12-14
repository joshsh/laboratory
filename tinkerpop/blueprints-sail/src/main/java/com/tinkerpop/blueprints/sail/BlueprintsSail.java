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
import java.util.LinkedList;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * User: josh
 * Date: Dec 11, 2010
 * Time: 12:59:54 PM
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
            URI_PREFIX = 'u',
            BLANK_NODE_PREFIX = 'b',
            PLAIN_LITERAL_PREFIX = 'p',
            TYPED_LITERAL_PREFIX = 't',
            LANGUAGE_TAG_LITERAL_PREFIX = 'l',
            NULL_CONTEXT_PREFIX = 'n';

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

    private final Indexes indexes = new Indexes();

    private final Index<Edge> edges;

    public BlueprintsSail(final IndexableGraph graph) {
        this(graph, "s,p,o,c,sp,so,sc,po,pc,oc,spo,spc,soc,poc,spoc");
    }

    public BlueprintsSail(final IndexableGraph graph,
                          final String tripleIndexes) {
        indexes.graph = graph;

        // For now, use the default EDGES and VERTICES indices, which *must exist* in Blueprints and are automatically indexed.
        // Think harder about collision issues (if someone hands a non-empty, non-Sail graph to this constructor) later on.
        edges = graph.getIndex(Index.EDGES, Edge.class);

        indexes.namespaces = graph.getVertex(NAMESPACES_VERTEX_ID);
        if (null == indexes.namespaces) {
            indexes.namespaces = graph.addVertex(NAMESPACES_VERTEX_ID);
        }

        indexes.matchers[0] = new TrivialTriplePatternMatcher(edges, graph);

        parseTripleIndices(tripleIndexes);
        assignUnassignedTriplePatterns();

        indexes.manualTransactions = indexes.graph instanceof TransactionalGraph
                && TransactionalGraph.Mode.MANUAL == ((TransactionalGraph) indexes.graph).getTransactionMode();

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


        return new BlueprintsSailConnection(indexes);
    }

    public ValueFactory getValueFactory() {
        return indexes.valueFactory;
    }

    ////////////////////////////////////////////////////////////////////////////

    public class Indexes {
        public IndexableGraph graph;

        // We don't need a special ValueFactory implementation.
        public final ValueFactory valueFactory = new ValueFactoryImpl();

        public final Collection<TriplePatternMatcher> indexingMatchers = new LinkedList<TriplePatternMatcher>();

        // A triple pattern matcher for each spoc combination
        public final TriplePatternMatcher[] matchers = new TriplePatternMatcher[16];

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

        for (String s : u) {
            createMatcher(s);
        }
    }

    private void assignUnassignedTriplePatterns() {
        TriplePatternMatcher[] n = new TriplePatternMatcher[16];
        n[0] = indexes.matchers[0];

        for (String[] alts : ALTERNATIVES) {
            String p = alts[0];
            int i = indexFor(p);

            TriplePatternMatcher m = indexes.matchers[i];

            // if no matcher has been assigned for this pattern
            if (null == m) {
                // try primary alternatives in the order they are specified
                for (int j = 1; j < alts.length; j++) {
                    m = indexes.matchers[indexFor(alts[j])];
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

        System.arraycopy(n, 0, indexes.matchers, 0, 16);
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

    private void createMatcher(final String pattern) {
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
        TriplePatternMatcher m = new TriplePatternMatcher(edges, s, p, o, c);
        indexes.matchers[index] = m;

        //if (pattern.length() > 1) {
            indexes.indexingMatchers.add(m);
        //}
    }
}
