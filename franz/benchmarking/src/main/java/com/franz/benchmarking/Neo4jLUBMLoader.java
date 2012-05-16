package com.franz.benchmarking;

import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.pgm.oupls.sail.GraphSail;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Neo4jLUBMLoader {
    public static void main(final String[] args) {
        try {
            performLoad();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void performLoad() throws Exception {
        Neo4jGraph g = new Neo4jGraph("neo4j-lubm");
        g.setMaxBufferSize(1000);
        try {
            GraphSail sail = new GraphSail(g);
            sail.enforceUniqueStatements(false);
            sail.useVolatileStatements(false);
            sail.initialize();

            try {
                Repository repo = new SailRepository(sail);

                RepositoryConnection rc = repo.getConnection();
                try {
                    rc.setAutoCommit(false);
                    rc.clear();
                    rc.commit();

                    File lubmDir = new File("/net/foray/x/gwking/sources/lubm-1000-0.nt");
                    if (lubmDir.isDirectory()) {
                        for (File f : lubmDir.listFiles()) {
                            System.out.println("" + System.currentTimeMillis() + "\tloading " + f);

                            InputStream in = new FileInputStream(f);
                            try {
                                rc.add(in, "", RDFFormat.NTRIPLES);
                            } finally {
                                in.close();
                            }

                            rc.commit();
                        }
                    }
                } finally {
                    rc.close();
                }
            } finally {
                sail.shutDown();
            }
        } finally {
            g.shutdown();
        }
    }
}
