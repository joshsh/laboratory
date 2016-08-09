package com.franz.benchmarking;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.oupls.sail.GraphSail;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

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
        Runtime r = Runtime.getRuntime();

        Map<String, String> config = new HashMap<String, String>();
        config.put("neostore.nodestore.db.mapped_memory", "10M");
        config.put("string_block_size", "60");
        config.put("array_block_size", "300");

        Neo4jGraph g = new Neo4jGraph("neo4j-lubm", config);
        try {
            //g.setMaxBufferSize(1000);

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
                    //File lubmDir = new File("/tmp/neo4j-rdf");
                    if (lubmDir.isDirectory()) {
                        for (File f : lubmDir.listFiles()) {
                            System.out.println("" + System.currentTimeMillis()
                                    + "\t" + (r.totalMemory() - r.freeMemory())
                                    + "\t" + r.freeMemory()
                                    + "\t" + r.totalMemory()
                                    + "\t" + f);

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
