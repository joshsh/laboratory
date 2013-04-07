package com.tinkerpop.blueprints.oupls.sail;

import com.thinkaurelius.titan.core.TitanFactory;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TitanGraphSailTest extends GraphSailTest {
    protected KeyIndexableGraph createGraph() throws Exception {
        Configuration conf = new BaseConfiguration();

        //conf.setProperty("storage.backend", "cassandra");
        //conf.setProperty("storage.hostname", "127.0.0.1");

        //conf.setProperty("storage.directory", "/tmp/titan2");
        //conf.setProperty("storage.backend", "berkeleyje");

        conf.setProperty("storage.backend","hbase");

        return TitanFactory.open(conf);
    }
}
