package com.franz.benchmarking;

import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.pgm.oupls.sail.GraphSail;
import info.aduna.iteration.CloseableIteration;
import org.neo4j.cypher.ExecutionEngine;
import org.neo4j.cypher.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class CypherPlay {
    public static void main(final String[] args) {
        try {
            play();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    /*
    (defun test ()
    (aselect0 (?prof ?stud ?coll)
    (a ?prof !rdf:type !lubm:FullProfessor)
    (a ?prof !lubm:undergraduateDegreeFrom ?coll)
    (a ?stud !lubm:advisor ?prof)
    (a ?stud !lubm:undergraduateDegreeFrom ?coll)))
     */
    private static void play() throws Exception {
        System.out.println("## before ##");
        printMemoryInfo();

        Neo4jGraph g = new Neo4jGraph("neo4j-lubm");
        try {
            GraphDatabaseService service = g.getRawGraph();
            ExecutionEngine engine = new ExecutionEngine(service);

            GraphSail sail = new GraphSail(g);
            sail.initialize();
            SailConnection sc = sail.getConnection();
            CloseableIteration<? extends Statement, SailException> iter
                    = sc.getStatements(new URIImpl("http://tinkerpop.com#1"), null, null, false);
            System.out.println("statements:");
            while (iter.hasNext()) {
                System.out.println("\tstatement: " + iter.next());
            }
            iter.close();
            sc.close();


            System.out.println("nodes:");
            for (Node node : service.getAllNodes()) {
                System.out.println("\tnode: " + node);
                for (String key : node.getPropertyKeys()) {
                    System.out.println("\t\t" + key + ": " + node.getProperty(key));
                }
            }

            System.out.println("## during ##");
            printMemoryInfo();

//            String q = "start n=node(2) return n, n.kind";
            //String q = "START marko=node(2)\n" +
            //        "MATCH marko-[:`http://tinkerpop.com#knows`]->fof\n" +
            //        "RETURN marko, fof, fof.kind";

            String q = "START josh=node:vertices(value = \"http://tinkerpop.com#4\")\n" +
                    "MATCH josh<-[:`http://tinkerpop.com#knows`]-fof\n" +
                    "RETURN josh, fof, fof.kind";

            ExecutionResult result = engine.execute(q);

            System.out.println("result: " + result.dumpToString());

            System.out.println("## after queries ##");
            printMemoryInfo();
        } finally {
            g.shutdown();

            System.out.println("## after shutdown ##");
            printMemoryInfo();
        }
    }

    private static final Runtime RUNTIME = Runtime.getRuntime();

    public static void printMemoryInfo() {
        System.out.println("memory info:");
        System.out.println("\tmax:\t" + RUNTIME.maxMemory());
        System.out.println("\ttotal:\t" + RUNTIME.totalMemory());
        System.out.println("\tfree:\t" + RUNTIME.freeMemory());
        System.out.println("\tused:\t" + (RUNTIME.totalMemory() - RUNTIME.freeMemory()));
    }
}
