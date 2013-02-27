package net.fortytwo;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.Sail;

import java.io.File;
import java.io.FileInputStream;
import java.util.zip.GZIPInputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DBPediaLoader {
    private static final String SOURCE = "/data/datasets/dbpedia/necessary/";
    private static final String DEST = "/data/tmp/bps-neo4j-dbpedia";

    public static void main(final String[] args) {
        try {
            new DBPediaLoader().load();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private void load() throws Exception {
        Neo4jGraph g = new Neo4jGraph(DEST);

        Sail sail = new GraphSail(g);
        sail.initialize();
        try {
            Repository repo = new SailRepository(sail);
            RepositoryConnection rc = repo.getConnection();
//            rc.setAutoCommit(false);
            rc.setAutoCommit(true);
            try {
                rc.clear();
                rc.commit();

                File dir = new File(SOURCE);
                for (File f : dir.listFiles()) {
                    System.out.println("loading file: " + f);
                    GZIPInputStream is = new GZIPInputStream(new FileInputStream(f));
                    try {
                        rc.add(is, "", RDFFormat.NTRIPLES);
                    } catch (Throwable t) {
                        // Attempt to recover.
                        t.printStackTrace(System.err);
                    } finally {
                        is.close();
                    }
                    rc.commit();
                }
            } finally {
                rc.close();
            }
        } finally {
            sail.shutDown();
        }
    }
}
