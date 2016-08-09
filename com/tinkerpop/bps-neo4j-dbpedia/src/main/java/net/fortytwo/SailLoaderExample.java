package net.fortytwo;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import com.tinkerpop.blueprints.oupls.sail.SailLoader;
import org.openrdf.sail.Sail;

import java.io.File;

public class SailLoaderExample {
    public static void main(final String[] args) throws Exception {
        Neo4jGraph g = new Neo4jGraph("/tmp/wordnet/neo4j");

        Sail sail = new GraphSail(g);
        sail.initialize();
        try {
            SailLoader loader = new SailLoader(sail);
            loader.setVerbose(true);
            loader.load(new File("/tmp/wordnet/rdf"));
        } finally {
            sail.shutDown();
        }
    }
}
