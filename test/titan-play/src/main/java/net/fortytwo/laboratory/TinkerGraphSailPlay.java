package net.fortytwo.laboratory;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import info.aduna.iteration.CloseableIteration;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;
import org.openrdf.sail.inferencer.fc.ForwardChainingRDFSInferencer;

public class TinkerGraphSailPlay {
    public static void main(final String[] args) throws Exception {
        inferencePlay();
    }

    private static void addStatementsPlay() throws SailException {
        TinkerGraph graph = new TinkerGraph();
        Sail sail = new GraphSail(graph);
        sail.initialize();
        SailConnection sc = sail.getConnection();
        sc.begin();

        ValueFactory vf = sail.getValueFactory();
        sc.addStatement(vf.createURI("http://tinkerpop.com#1"), vf.createURI("http://tinkerpop.com#knows"), vf.createURI("http://tinkerpop.com#3"), vf.createURI("http://tinkerpop.com"));
        sc.addStatement(vf.createURI("http://tinkerpop.com#1"), vf.createURI("http://tinkerpop.com#name"), vf.createLiteral("marko"), vf.createURI("http://tinkerpop.com"));
        sc.addStatement(vf.createURI("http://tinkerpop.com#3"), vf.createURI("http://tinkerpop.com#name"), vf.createLiteral("josh"), vf.createURI("http://tinkerpop.com"));

        System.out.println("get statements: ?s ?p ?o ?g");
        CloseableIteration<? extends Statement, SailException> results = sc.getStatements(null, null, null, false);
        while (results.hasNext()) {
            System.out.println(results.next());
        }

        System.out.println("\nget statements: http://tinkerpop.com#3 ?p ?o ?g");
        results = sc.getStatements(vf.createURI("http://tinkerpop.com#3"), null, null, false);
        while (results.hasNext()) {
            System.out.println(results.next());
        }

        sc.rollback();
        sc.close();
        graph.shutdown();
        sail.shutDown();
    }

    private static void inferencePlay() throws SailException {
        Resource beijing = new URIImpl("http://example.org/things/Beijing");
        Resource city = new URIImpl("http://example.org/terms/city");
        Resource place = new URIImpl("http://example.org/terms/place");

        KeyIndexableGraph graph = new TinkerGraph();
        Sail reasoner = new ForwardChainingRDFSInferencer(new GraphSail(graph));
        reasoner.initialize();

        try {
            SailConnection c = reasoner.getConnection();
            c.begin();
            try {
                c.addStatement(city, RDFS.SUBCLASSOF, place);
                c.addStatement(beijing, RDF.TYPE, city);
                c.commit();
                c.begin();

                CloseableIteration<? extends Statement, SailException> i
                        = c.getStatements(beijing, null, null, true);
                try {
                    while (i.hasNext()) {
                        System.out.println("statement " + i.next());
                    }
                } finally {
                    i.close();
                }
            } finally {
                c.rollback();
                c.close();
            }
        } finally {
            reasoner.shutDown();
        }

        graph.shutdown();
    }
}
