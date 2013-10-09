package net.fortytwo.laboratory;

import com.thinkaurelius.titan.core.TitanFactory;
import com.thinkaurelius.titan.core.TitanGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import info.aduna.iteration.CloseableIteration;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.openrdf.model.Statement;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TitanGraphSailPlay {
    public static void main(final String[] args) throws Exception {
        Configuration conf = new BaseConfiguration();

        conf.setProperty("storage.backend", "cassandra");
        conf.setProperty("storage.hostname", "127.0.0.1");

        //conf.setProperty("storage.directory", "/tmp/titan2");
        //conf.setProperty("storage.backend", "berkeleyje");

        //conf.setProperty("storage.backend","hbase");

        TitanGraph g = TitanFactory.open(conf);
        try {
            GraphSail sail = new GraphSail(g);
//            GraphSail sail = new GraphSail(g, "");
            sail.initialize();
            try {
                SailConnection sc = sail.getConnection();
                try {
                    sc.clear();
                    //sc.commit();

                    sc.addStatement(RDF.TYPE, RDF.TYPE, RDF.TYPE);

                    CloseableIteration<? extends Statement, SailException> iter = sc.getStatements(RDF.TYPE, null, null, false);
                    while (iter.hasNext()) {
                        System.out.println("result: " + iter.next());
                    }

                    sc.commit();
                } finally {
                    sc.close();
                }
            } finally {
                sail.shutDown();
            }
        } finally {
            g.shutdown();
        }
    }
}
