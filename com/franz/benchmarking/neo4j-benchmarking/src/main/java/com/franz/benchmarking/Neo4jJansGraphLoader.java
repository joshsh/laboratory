package com.franz.benchmarking;

import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class Neo4jJansGraphLoader {
    private static final String NODE_URI_PREFIX = "http://franz.com/node-";

    public static final int TOTAL_NODES = 10000000;
    public static final int TOTAL_EDGES = 100000000;
    public static final String[] PREDICATES = {"pred-0", "pred-1", "pred-2", "pred-3", "pred-4", "pred-5", "pred-6", "pred-7", "pred-8", "pred-9"};

    public static void main(final String[] args) {
        RANDOM.setSeed(System.currentTimeMillis());

        try {
            performLoad();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    public static Neo4jGraph getOrCreateNeo4jGraph() {
        Map<String, String> config = new HashMap<String, String>();
        config.put("neostore.nodestore.db.mapped_memory", "10M");
        config.put("string_block_size", "60");
        config.put("array_block_size", "300");

        return new Neo4jGraph("/disk1/josh/neo4j-jans-graph", config);
    }

    /* On blade3, node creation took around 50s, while edge creation took around 96612s (27 hours) */
    private static void performLoad() throws Exception {
//        Neo4jGraph g = new Neo4jGraph("/tmp/neo4j-jans-graph", config);
        Neo4jGraph g = getOrCreateNeo4jGraph();
        try {
            //g.setMaxBufferSize(1000);
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
            for (int j = 0; j < TOTAL_EDGES; j++) {
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

    public static Vertex getRandomVertex(final Graph g) {
        while (true) {
            int id = RANDOM.nextInt(TOTAL_NODES);
            Vertex v = g.getVertex(id);
            if (null == v) {
                System.err.println("null vertex for id " + id);
            } else {
                return v;
            }
        }
    }

    private static Edge createRandomEdge(final Graph g) {
        Vertex head = getRandomVertex(g);
        Vertex tail = getRandomVertex(g);

        return g.addEdge(null, tail, head, PREDICATES[RANDOM.nextInt(10)]);
    }
}
