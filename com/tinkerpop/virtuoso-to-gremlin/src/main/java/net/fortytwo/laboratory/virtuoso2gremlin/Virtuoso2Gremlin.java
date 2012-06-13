package net.fortytwo.laboratory.virtuoso2gremlin;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.sail.SailGraph;
import com.tinkerpop.gremlin.Gremlin;
import com.tinkerpop.pipes.Pipe;
import com.tinkerpop.pipes.util.SingleIterator;
import net.fortytwo.flow.Collector;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.ripple.model.impl.sesame.SesameModel;
import net.fortytwo.ripple.query.QueryEngine;
import net.fortytwo.ripple.query.QueryPipe;
import net.fortytwo.sesametools.readonly.ReadOnlySail;
import net.fortytwo.sesametools.reposail.RepositorySail;
import org.openrdf.repository.Repository;
import org.openrdf.repository.http.HTTPRepository;
import org.openrdf.sail.Sail;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Virtuoso2Gremlin {
    static {
        Gremlin.load();
    }

    public static void main(final String[] args) throws Exception {

        String sesameServer = "http://data.oceandrilling.org/openrdf-sesame";
        String repositoryID = "COLSesRep";

        Repository myRepository = new HTTPRepository(sesameServer, repositoryID);

        Sail baseSail = new RepositorySail(myRepository);
        baseSail.initialize();
        Sail sail = new ReadOnlySail(baseSail);

        /*
        SailConnection sc = sail.getConnection();
        try {
            sc.getStatements(new URIImpl("http://data.oceandrilling.org/codices/lsh/117"), null, null, false);
        } finally {
            sc.close();
        }
         System.exit(0);
        //*/

        QueryEngine q = new QueryEngine(new SesameModel(sail));
        Collector<RippleList> results = new Collector<RippleList>();
        QueryPipe p = new QueryPipe(q, results);
        p.put("<http://data.oceandrilling.org/codices/lsh/117> links.\n");
        for (RippleList l : results) {
            System.out.println("result: " + l);
        }
        //*/

        //*
        Graph g = new SailGraph(sail);

        Pipe<Vertex, Vertex> pipe = Gremlin.compile("_().outE.inV");
        pipe.setStarts(new SingleIterator<Vertex>(g.getVertex("http://data.oceandrilling.org/codices/lsh/117")));
        for (Object obj : pipe) {
            System.out.println(obj);
        }//*/
    }
}
