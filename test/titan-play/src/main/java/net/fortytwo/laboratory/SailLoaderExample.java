package net.fortytwo.laboratory;

import com.tinkerpop.blueprints.impls.neo4j2.Neo4j2Graph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import com.tinkerpop.blueprints.oupls.sail.SailLoader;

import java.io.File;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SailLoaderExample {
    public static void main(final String[] args) throws Exception {
        new SailLoaderExample().loadFile();
    }

    private final String DB_DIRECTORY = "/tmp/neo";

    public void loadFile() throws Exception {
        File f = new File("/tmp/sp2bench-1000000.nt.gz");

        Neo4j2Graph neo4jGraph = new Neo4j2Graph(this.DB_DIRECTORY);
        try {
            GraphSail sail = new GraphSail(neo4jGraph);
            sail.initialize();

            try {
                SailLoader loader = new SailLoader(sail);
                loader.setBufferSize(1000);  // this is the default
                loader.setVerbose(true);
                loader.load(f);
            } finally {
                sail.shutDown();
            }
        } finally {
            neo4jGraph.shutdown();
        }
    }
}
