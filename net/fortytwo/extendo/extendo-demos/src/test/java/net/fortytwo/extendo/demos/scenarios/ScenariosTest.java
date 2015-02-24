package net.fortytwo.extendo.demos.scenarios;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.SesameStream;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.rdfagents.model.Dataset;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ScenariosTest {
    private static final int TUPLE_TTL = 0, QUERY_TTL = 0;

    private QueryEngineImpl queryEngine = new QueryEngineImpl();

    protected ValueFactory vf = new ValueFactoryImpl();

    protected String ex = "http://example.org/";
    protected org.openrdf.model.URI arthur = vf.createURI(ex + "arthur");
    protected org.openrdf.model.URI zaphod = vf.createURI(ex + "zaphod");
    protected org.openrdf.model.URI ford = vf.createURI(ex + "ford");
    protected org.openrdf.model.URI book = vf.createURI(ex + "thebook");

    private long now;

    @Before
    public void setUp() throws Exception {
        SesameStream.setDoPerformanceMetrics(true);
        now = System.currentTimeMillis();
        queryEngine.clear();
    }

    @After
    public void tearDown() throws Exception {

    }

    private List<BindingSet> add(final String query)
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {

        final List<BindingSet> results = new LinkedList<BindingSet>();
        queryEngine.addQuery(QUERY_TTL, query, new BindingSetHandler() {
            @Override
            public void handle(BindingSet b) {
                results.add(b);
            }
        });

        return results;
    }

    @Test
    public void testQueryForThingsPointedTo() throws Exception {

        assertEquals(0, queryEngine.get(QueryEngineImpl.Quantity.Queries));
        assertEquals(0, queryEngine.get(QueryEngineImpl.Quantity.Statements));
        assertEquals(0, queryEngine.get(QueryEngineImpl.Quantity.Solutions));

        List<BindingSet> thingsPointedToResults = add(Activities.QUERY_FOR_THINGS_POINTED_TO);

        assertEquals(1, queryEngine.get(QueryEngineImpl.Quantity.Queries));
        assertEquals(0, queryEngine.get(QueryEngineImpl.Quantity.Statements));
        assertEquals(0, queryEngine.get(QueryEngineImpl.Quantity.Solutions));

        // Arthur points to an object.  One solution.

        queryEngine.addStatements(TUPLE_TTL,
                toArray(Activities.datasetForPointingGesture(now, arthur, book)));

        assertEquals(1, queryEngine.get(QueryEngineImpl.Quantity.Queries));
        assertEquals(6, queryEngine.get(QueryEngineImpl.Quantity.Statements));
        assertEquals(1, queryEngine.get(QueryEngineImpl.Quantity.Solutions));

        assertEquals(1, thingsPointedToResults.size());
        assertEquals(arthur, thingsPointedToResults.get(0).getValue("actor"));
        assertEquals(book, thingsPointedToResults.get(0).getValue("indicated"));

        thingsPointedToResults.clear();

        // Arthur points to another object.  One more solution.

        queryEngine.addStatements(TUPLE_TTL,
                toArray(Activities.datasetForPointingGesture(now, arthur, ford)));

        assertEquals(1, queryEngine.get(QueryEngineImpl.Quantity.Queries));
        assertEquals(12, queryEngine.get(QueryEngineImpl.Quantity.Statements));
        assertEquals(2, queryEngine.get(QueryEngineImpl.Quantity.Solutions));

        assertEquals(1, thingsPointedToResults.size());
        assertEquals(arthur, thingsPointedToResults.get(0).getValue("actor"));
        assertEquals(ford, thingsPointedToResults.get(0).getValue("indicated"));

        thingsPointedToResults.clear();

        // Zaphod points to an object.  A third solution.

        queryEngine.addStatements(TUPLE_TTL,
                toArray(Activities.datasetForPointingGesture(now, zaphod, book)));

        assertEquals(1, queryEngine.get(QueryEngineImpl.Quantity.Queries));
        assertEquals(18, queryEngine.get(QueryEngineImpl.Quantity.Statements));
        assertEquals(3, queryEngine.get(QueryEngineImpl.Quantity.Solutions));

        assertEquals(1, thingsPointedToResults.size());
        assertEquals(zaphod, thingsPointedToResults.get(0).getValue("actor"));
        assertEquals(book, thingsPointedToResults.get(0).getValue("indicated"));
    }

    private Statement[] toArray(Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

}
