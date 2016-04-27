package net.fortytwo.laboratory;

import com.thinkaurelius.titan.core.TitanFactory;
import com.thinkaurelius.titan.core.TitanGraph;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.IndexableGraph;
import com.tinkerpop.blueprints.impls.neo4j2.Neo4j2Graph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import com.tinkerpop.blueprints.oupls.sail.SailLoader;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;

import java.io.File;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TitanSailLoaderExample {
    public static void main(final String[] args) throws Exception {
        new TitanSailLoaderExample().loadFile();
    }

    public void loadFile() throws Exception {
        Configuration conf = new BaseConfiguration();
        conf.setProperty("storage.backend", "cassandra");
        conf.setProperty("storage.hostname", "127.0.0.1");
        TitanGraph g = TitanFactory.open(conf);

        File f = new File("/tmp/graph-example-1.nt");

        try {
            GraphSail sail = new GraphSail(g);
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
            g.shutdown();
        }
    }
}
