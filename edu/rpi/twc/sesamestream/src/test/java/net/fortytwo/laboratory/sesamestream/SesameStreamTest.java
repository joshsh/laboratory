package net.fortytwo.laboratory.sesamestream;

import info.aduna.io.IOUtil;
import org.junit.Test;
import org.openrdf.query.BindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParser;
import org.openrdf.query.parser.sparql.SPARQLParser;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SesameStreamTest {
    private static final String
            SIMPLE_JOIN = "simple-join-in.rq",
            SIMPLE_SP = "simple-sp.rq";

    private static final String[] QUERIES = {"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q11", "q12", "q13", "q14"};

    private final QueryParser parser = new SPARQLParser();
    private final String baseUri = "http://example.org/ns/";

    @Test
    public void testParseLubmQueries() throws Exception {
        QueryEngine engine = new QueryEngine();

        BindingSetHandler bsh = new BindingSetHandler() {
            public void handle(BindingSet result) {
                // Do nothing.
            }
        };

        for (String q : QUERIES) {
            String name = "lubm/" + q + ".rq";
            System.out.println("loading query " + name);
            InputStream in = SesameStream.class.getResourceAsStream(name);
            try {
                String query = IOUtil.readString(in);
                ParsedQuery pq = parser.parseQuery(query, baseUri);
                engine.addQuery(pq.getTupleExpr(), bsh);
            }   finally {
                in.close();
            }
        }
    }

    @Test
    public void testNothingMuch() throws Exception {
        String query = getQuery(SIMPLE_JOIN);

        ParsedQuery pq = parser.parseQuery(query, baseUri);
        System.out.println("query : " + pq);
    }

    private String getQuery(final String fileName) throws IOException {
        return new String(IOUtil.readBytes(SesameStream.class.getResourceAsStream(fileName)));
    }
}
