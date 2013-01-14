package com.franz.benchmarking;

import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;
import org.neo4j.cypher.ExecutionEngine;
import org.neo4j.cypher.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;

import java.util.Iterator;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Neo4jJansGraphCypherBenchmark {
    private static final int ITERATIONS = 10000;

    private static final String QUERY_TEMPLATE = "START x=node(X_ID)\n" +
//            "MATCH x-[?:p]->y, y-[?:q]->z\n" +
            "MATCH x-[:`REL`]->y, y-[:`REL`]->z, z-[:`REL`]->a, a-[:`REL`]->x\n" +
            "WHERE (a != y)\n" +
            "RETURN x, y, z, a";

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

                for (String type : Neo4jJansGraphLoader.PREDICATES) {
                    String query = QUERY_TEMPLATE
                            .replace("X_ID", xId.toString())
                            .replace("REL", type);

                    ExecutionResult result = engine.execute(query);

                    Iterator<Map<String, Object>> iter = result.javaIterator();
                    if (iter.hasNext()) {
                        StringBuilder sb = new StringBuilder();
                        for (Map.Entry<String, Object> e : iter.next().entrySet()) {
                            sb.append(" ").append(e.getKey()).append(":").append(e.getValue());
                        }
                        System.out.println("result: " + sb);
                    }
                }

                if (0 == i % 1000) {
                    CypherPlay.printMemoryInfo();
                }
            }
        } finally {
            g.shutdown();
        }
    }
}
