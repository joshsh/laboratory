package com.franz.benchmarking;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphFactory {
    public static TinkerGraph createTinkerGraph() {
        return new TinkerGraph();
    }

    public static Neo4jGraph createNeo4jGraph(final String dir) {
        Neo4jGraph g = new Neo4jGraph(dir);
        g.getRawGraph().index().getNodeAutoIndexer().startAutoIndexingProperty(IdGraph.ID);
        return g;
    }
}
