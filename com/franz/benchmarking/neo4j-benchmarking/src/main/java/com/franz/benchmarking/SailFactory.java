package com.franz.benchmarking;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import org.openrdf.sail.Sail;
import org.openrdf.sail.nativerdf.NativeStore;

import java.io.File;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SailFactory {
    public Sail createSail(final String type,
                           final String directory) {
        String tol = type.toLowerCase();

        if (tol.equals("nativestore")) {
            return new NativeStore(new File(directory));
        } else if (tol.equals("graphsail-neo4j")) {
            Neo4jGraph graph = new Neo4jGraph(directory);

            GraphSail sail = new GraphSail(graph);
            sail.enforceUniqueStatements(false);
            sail.useVolatileStatements(false);

            return sail;
        } else {
            throw new IllegalArgumentException("unknown Sail type: " + type);
        }
    }
}
