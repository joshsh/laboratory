package edu.rpi.twc.sesamestream;

import edu.rpi.twc.sesamestream.util.StatementListBuilder;
import info.aduna.io.IOUtil;
import info.aduna.iteration.CloseableIteration;
import net.fortytwo.sesametools.nquads.NQuadsParser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.algebra.TupleExpr;
import org.openrdf.query.impl.DatasetImpl;
import org.openrdf.query.impl.EmptyBindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParser;
import org.openrdf.query.parser.sparql.SPARQLParser;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.turtle.TurtleParser;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.memory.MemoryStore;

import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertTrue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QueryEngineTest {
    private static final String BASE_URI = "http://example.org/base/";

    private QueryParser queryParser = new SPARQLParser();
    private Sail sail;
    private QueryEngine queryEngine;

    @Before
    public void setUp() throws Exception {
        sail = new MemoryStore();
        sail.initialize();

        queryEngine = new QueryEngine();
    }

    @After
    public void tearDown() throws Exception {
        sail.shutDown();
    }

    @Test
    public void testSimple() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("simple-sp.rq"));
    }

    @Test
    public void testSingleJoinIn() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("simple-join-in.rq"));
    }

    @Test
    public void testSingleJoinOut() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("simple-join-out.rq"));
    }

    @Test
    public void testUnselectedVariables() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("unselected-variables.rq"));
    }

    @Test
    public void testProjection() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("simple-sp-with-proj.rq"));

        compareAnswers(
                loadData("example.nq"),
                loadQuery("simple-join-out-with-proj.rq"));
    }

    @Test
    public void testSimultaneousQueries() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("simple-join-in.rq"),
                loadQuery("simple-join-out.rq"));
    }

    @Test
    public void testMultipleJoins() throws Exception {
        compareAnswers(
                loadData("example.nq"),
                loadQuery("multiple-join-1.rq"));
    }

    @Test
    public void testTmp() throws Exception {
        compareAnswers(
                loadData("universe.ttl"),
                loadQuery("universe-joins-1.rq"));
    }

    private TupleExpr loadQuery(final String fileName) throws Exception {
        InputStream in = SesameStream.class.getResourceAsStream(fileName);
        String query = IOUtil.readString(in);
        in.close();

        ParsedQuery pq = queryParser.parseQuery(query, BASE_URI);

        return pq.getTupleExpr();
    }

    private List<Statement> loadData(final String fileName) throws Exception {
        RDFParser p;
        if (fileName.endsWith("nq")) {
            p = new NQuadsParser();
        } else if (fileName.endsWith("ttl")) {
            p = new TurtleParser();
        } else {
            throw new IllegalStateException("unsupported file extension");
        }

        StatementListBuilder c = new StatementListBuilder();
        p.setRDFHandler(c);

        InputStream in = SesameStream.class.getResourceAsStream(fileName);
        p.parse(in, BASE_URI);
        in.close();

        return c.getStatements();
    }

    private void compareAnswers(final List<Statement> data,
                                final TupleExpr... queries) throws Exception {
        Set<BindingSet>[] staticResults = staticQueryAnswers(data, queries);
        Set<BindingSet>[] contResults = continuousQueryAnswers(data, queries);

        for (int i = 0; i < queries.length; i++) {
            Set<BindingSet> staticR = staticResults[i];
            Set<BindingSet> contR = contResults[i];

            for (BindingSet b : staticR) {
                assertTrue("expected result not found for query " + i + ": " + b, contR.contains(b));
            }

            for (BindingSet b : contR) {
                assertTrue("unexpected result for query " + i + ": " + b, staticR.contains(b));
            }
        }
    }

    private Set<BindingSet>[] staticQueryAnswers(final List<Statement> data,
                                                 final TupleExpr... queries) throws Exception {
        Set<BindingSet>[] answers = new Set[queries.length];

        int i = 0;
        for (TupleExpr query : queries) {
            Set<BindingSet> results = new HashSet<BindingSet>();
            answers[i] = results;

            SailConnection sc = sail.getConnection();
            try {
                for (Statement s : data) {
                    sc.addStatement(s.getSubject(), s.getPredicate(), s.getObject(), s.getContext());
                }

                CloseableIteration<? extends BindingSet, QueryEvaluationException> iter
                        = sc.evaluate(query, new DatasetImpl(), new EmptyBindingSet(), false);
                try {
                    while (iter.hasNext()) {
                        results.add(iter.next());
                    }
                } finally {
                    iter.close();
                }
            } finally {
                sc.rollback();
                sc.close();
            }

            i++;
        }

        return answers;
    }

    private Set<BindingSet>[] continuousQueryAnswers(final List<Statement> data,
                                                     final TupleExpr... queries) throws Exception {
        final Set<BindingSet>[] answers = new Set[queries.length];

        queryEngine.clear();

        int i = 0;
        for (TupleExpr t : queries) {
            answers[i] = new HashSet<BindingSet>();

            final int id = i;
            BindingSetHandler h = new BindingSetHandler() {
                private final Set<BindingSet> a = answers[id];

                public void handle(BindingSet result) {
                    a.add(result);
                }
            };

            queryEngine.addQuery(t, h);

            i++;
        }

        for (Statement s : data) {
            queryEngine.addStatement(s);
        }

        return answers;
    }

}
