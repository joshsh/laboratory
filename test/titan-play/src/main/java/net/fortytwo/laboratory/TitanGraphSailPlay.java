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
        String patterns = null;

        GraphFactory f = new GraphFactory();
        TitanGraph g = f.createTitanOnCassandra("127.0.0.1", null);
        patterns = "p,c,pc,s,sp,op,poc";

        try {
            GraphSail sail = null == patterns ? new GraphSail(g) : new GraphSail(g, patterns);
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
