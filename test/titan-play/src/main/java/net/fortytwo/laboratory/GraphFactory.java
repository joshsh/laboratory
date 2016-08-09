package net.fortytwo.laboratory;

import com.thinkaurelius.titan.core.TitanFactory;
import com.thinkaurelius.titan.core.TitanGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;

import java.io.File;

public class GraphFactory {
    public TinkerGraph createTinkerGraph() {
        return new TinkerGraph();
    }

    public TitanGraph createTitanOnCassandra(final String host,
                                             final String keyspace) {
        Configuration conf = new BaseConfiguration();

        conf.setProperty("storage.backend", "cassandra");
        conf.setProperty("storage.hostname", host);
        if (null != keyspace) {
            conf.setProperty("storage.keyspace", keyspace);
        }
        conf.setProperty("storage.batch-loading", "true");

        return TitanFactory.open(conf);
    }

    public TitanGraph createTitanOnBerkeleyJE(final File dir,
                                              final boolean batchLoading) {

        Configuration conf = new BaseConfiguration();
        conf.setProperty("storage.backend", "berkeleyje");
        conf.setProperty("storage.directory", dir.getAbsolutePath());

        if (batchLoading) {
            conf.setProperty("storage.batch-loading", true);
        }

        return TitanFactory.open(conf);
    }

    public TitanGraph createTitanOnHBase() {
        Configuration conf = new BaseConfiguration();

        conf.setProperty("storage.backend", "hbase");

        return TitanFactory.open(conf);
    }
}
