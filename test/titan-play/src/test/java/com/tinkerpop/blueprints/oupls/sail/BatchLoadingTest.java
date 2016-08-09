package com.tinkerpop.blueprints.oupls.sail;

import com.thinkaurelius.titan.core.TitanGraph;
import net.fortytwo.laboratory.GraphFactory;
import org.junit.Test;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.repository.sail.SailRepositoryConnection;

import java.io.File;
import java.io.IOException;

public class BatchLoadingTest {
    @Test
    public void testBatchLoading() throws Exception {
        File dataDir = File.createTempFile("titanplay-", "-db");
        String path = dataDir.getAbsolutePath();
        dataDir.delete();
        dataDir = new File(path);
        if (!dataDir.mkdirs()) {
            throw new IOException("could not create temporary directory");
        }

        boolean batchLoading = true;
        TitanGraph g = new GraphFactory().createTitanOnBerkeleyJE(dataDir, batchLoading);
        GraphSail sail = new GraphSail(g);
        SailRepository sr = new SailRepository(sail);
        sr.initialize();
        ValueFactory vf = sr.getValueFactory();
        try {
            SailRepositoryConnection c = sr.getConnection();
            try {
                c.add(RDF.TYPE, RDF.TYPE, RDF.TYPE);
            } finally {
                c.close();
            }
        } finally {
            sr.shutDown();
        }
    }
}
