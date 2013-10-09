package com.franz.benchmarking.sp2bench;

import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.kernel.impl.util.StringLogger;

import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SP2BenchQueries {
    public static void main(final String[] args) throws Exception {
        /*
SELECT ?yr
WHERE {
  ?journal rdf:type bench:Journal .
  ?journal dc:title "Journal 1 (1940)"^^xsd:string .
  ?journal dcterms:issued ?yr
}
         */

        GraphDatabaseFactory factory = new GraphDatabaseFactory();
        GraphDatabaseService g = factory.newEmbeddedDatabase("/tmp/neo-sp2bench");
        try {
            // Q1
            String query = "START journal=node(*)" +
                    "MATCH journal-[:`rdf:type`]->bj, journal-[:`dc:title`]->title, journal-[:`dcterms:issued`]->year\n" +
                    "WHERE bj.__id = 'bench:Journal' AND title.__id = '\"Journal 1 (1940)\"'\n" +
                    "RETURN journal.__id, year.__id";

            ExecutionEngine engine = new ExecutionEngine(g, StringLogger.SYSTEM);
            Transaction tx = g.beginTx();
            long time1 = System.currentTimeMillis();
            ExecutionResult result = engine.execute(query);
            long time2 = System.currentTimeMillis();

            String rows = "";
            for (Map<String, Object> row : result) {
                for (Map.Entry<String, Object> column : row.entrySet()) {
                    rows += column.getKey() + ": " + column.getValue() + "; ";
                }
                rows += "\n";
            }
            System.out.print(rows);
            long time3 = System.currentTimeMillis();

            System.out.println("execution time: " + (time2 -time1) + "ms");
            System.out.println("iteration time: " + (time3 - time2) + "ms");

        } finally {
            g.shutdown();
        }

    }
}
