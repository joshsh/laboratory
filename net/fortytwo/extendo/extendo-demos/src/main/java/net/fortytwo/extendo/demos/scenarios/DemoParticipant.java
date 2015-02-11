package net.fortytwo.extendo.demos.scenarios;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DemoParticipant {
    private static final Logger logger = Logger.getLogger(DemoParticipant.class.getName());

    private static final String PARTICIPANT_URI = "http://fortytwo.net/josh/things/CybU2QN"; // Arthur Dent

    private final ExtendoAgent agent;

    private void shareAttention(final URI focus) throws IOException {
        logger.info("sharing attention on " + focus);

        Dataset d = Activities.datasetForAttentionActivity(System.currentTimeMillis(), agent.getAgentUri(), focus);
        agent.getQueryEngine().addStatements(Extendo.GESTURE_TTL, d.getStatements());
    }

    public DemoParticipant()
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {

        agent = new ExtendoAgent(PARTICIPANT_URI, true);

        agent.getQueryEngine().addQuery(Activities.QUERY_FOR_THINGS_POINTED_TO, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                Value actor = bindingSet.getValue("actor");
                Value indicated = bindingSet.getValue("indicated");

                System.out.println("got a result: " + actor + " pointed to " + indicated);

                if (indicated instanceof URI) {
                    try {
                        shareAttention((URI) indicated);
                    } catch (IOException e) {
                        logger.log(Level.WARNING, "failed to share attention", e);
                    }
                } else {
                    logger.warning("value indicated is not a URI: " + indicated);
                }
            }
        });
    }

    public static void main(final String[] args) throws Exception {
        DemoParticipant p = new DemoParticipant();

        // wait until killed
        Object lock = "";
        synchronized (lock) {
            lock.wait();
        }
    }
}
