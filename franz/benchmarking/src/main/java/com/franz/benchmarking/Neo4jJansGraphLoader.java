package com.franz.benchmarking;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Neo4jJansGraphLoader {
    private static final String NODE_URI_PREFIX = "http://franz.com/node-";

    private static final int TOTAL_NODES = 10000000;
    private static final int TOTAL_STATEMENTS = 100000000;
    private static final String[] PREDICATES = {"pred-0", "pred-1", "pred-2", "pred-3", "pred-4", "pred-5", "pred-6", "pred-7", "pred-8", "pred-9"};

    public static void main(final String[] args) {
        RANDOM.setSeed(System.currentTimeMillis());

        try {
            performLoad();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void performLoad() throws Exception {
        Runtime r = Runtime.getRuntime();

        Map<String, String> config = new HashMap<String, String>();
        config.put("neostore.nodestore.db.mapped_memory", "10M");
        config.put("string_block_size", "60");
        config.put("array_block_size", "300");

        Neo4jGraph g = new Neo4jGraph("/tmp/neo4j/jans-graph", config);
        try {
            g.setMaxBufferSize(1000);
            long start = System.currentTimeMillis();

            for (int i = 0; i < TOTAL_NODES; i++) {
                Vertex v = g.addVertex(null);
                assert (v.getId().equals(i));
                if (0 == i % 100000) {
                    long now = System.currentTimeMillis();
                    System.out.println("" + (i / 100000) + "% of nodes added in " + ((now - start) / 1000) + "s");
                }
            }

            start = System.currentTimeMillis();
            for (int j = 0; j < TOTAL_STATEMENTS; j++) {
                createRandomEdge(g);

                if (0 == j % 1000000) {
                    long now = System.currentTimeMillis();
                    System.out.println("" + (j / 1000000) + "% of statements added in " + ((now - start) / 1000) + "s");
                }
            }
        } finally {
            g.shutdown();
        }
    }

    private static Random RANDOM = new Random();

    private static Vertex randomVertex(final Graph g) {
        int a = RANDOM.nextInt(TOTAL_NODES);
        return g.getVertex(a);
    }

    private static Edge createRandomEdge(final Graph g) {
        Vertex head = randomVertex(g);
        Vertex tail = randomVertex(g);

        return g.addEdge(null, tail, head, PREDICATES[RANDOM.nextInt(10)]);
    }
}
