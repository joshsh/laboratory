package com.franz.benchmarking;

import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;
import org.neo4j.cypher.ExecutionEngine;
import org.neo4j.cypher.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Neo4jJansGraphCypherBenchmark {
    private static final int ITERATIONS = 10;

    private static final String QUERY_TEMPLATE = "START x=node(X_ID)\n" +
            "RETURN x";

    /*
   (time
(dotimes (i 10000)
 (let ((x (aref .nodes. (random .max-nodes.))))
   (select-distinct (?x ?pa)
     (:limit 1)
     (q (?? x) ?pa ?y)
     (q ?y ?pa ?z)
     (q ?z ?pa ?a)
     (notIs ?a ?y)
     (q ?a ?pa (?? x))))))
    */

    public static void main(final String[] args) throws Exception {
        Neo4jGraph g = Neo4jJansGraphLoader.getOrCreateNeo4jGraph();
        try {
            GraphDatabaseService service = g.getRawGraph();
            ExecutionEngine engine = new ExecutionEngine(service);

            for (int i = 0; i < ITERATIONS; i++) {
                Vertex x = Neo4jJansGraphLoader.getRandomVertex(g);
                Object xId = x.getId();

                String query = QUERY_TEMPLATE.replace("X_ID", xId.toString());

                ExecutionResult result = engine.execute(query);

                System.out.println("result: " + result.dumpToString());

                CypherPlay.printMemoryInfo();
            }
        } finally {
            g.shutdown();
        }
    }
}
