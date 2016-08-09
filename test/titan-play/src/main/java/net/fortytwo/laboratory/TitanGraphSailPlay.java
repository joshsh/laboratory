package net.fortytwo.laboratory;

import com.thinkaurelius.titan.core.TitanFactory;
import com.thinkaurelius.titan.core.TitanGraph;
import com.thinkaurelius.titan.core.TitanProperty;
import com.thinkaurelius.titan.core.TitanType;
import com.thinkaurelius.titan.graphdb.types.vertices.TitanKeyVertex;
import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import info.aduna.iteration.CloseableIteration;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.openrdf.model.Statement;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.io.File;

public class TitanGraphSailPlay {
    public static void main(final String[] args) throws Exception {
        Configuration conf = new BaseConfiguration();
        String patterns = null;

        GraphFactory f = new GraphFactory();
        //TitanGraph g = f.createTitanOnCassandra("127.0.0.1", null);
        TitanGraph g = f.createTitanOnBerkeleyJE(new File("/tmp/titan-play"), false);
        patterns = "p,c,pc,s,sp,op,poc";

        try {
            GraphSail sail = null == patterns ? new GraphSail(g) : new GraphSail(g, patterns);
            sail.initialize();


            for(String s : g.getIndexedKeys(Edge.class)) {
                System.out.println("indexed: " + s);
            }
            TitanType t = g.getType("c");
            System.out.println("t = " + t + " (" + t.getClass() + ")");

            try {
                SailConnection sc = sail.getConnection();
                try {
                    sc.begin();
                    sc.clear();
                    //sc.commit();

                    sc.addStatement(RDF.TYPE, RDF.TYPE, RDF.TYPE, RDF.TYPE);
                    sc.addStatement(RDFS.COMMENT, RDFS.COMMENT, RDFS.COMMENT, RDFS.COMMENT);

                    CloseableIteration<? extends Statement, SailException> iter = sc.getStatements(RDF.TYPE, null, null, false);
                    while (iter.hasNext()) {
                        System.out.println("result: " + iter.next());
                    }

                    sc.commit();

                    /*
                    for (TitanProperty p : ((TitanKeyVertex) t).getProperties()) {   ((TitanKeyVertex) t).
                        System.out.println("prop: " + p);
                    }*/

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
