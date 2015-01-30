package net.fortytwo.extendo.demos.scenarios;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.rdf.Activities;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DemoParticipant {
    private static final String PARTICIPANT_URI = "http://fortytwo.net/josh/things/CybU2QN"; // Arthur Dent

    private final ExtendoAgent agent;

    public DemoParticipant() throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {
        agent = new ExtendoAgent(PARTICIPANT_URI, true);

        agent.getQueryEngine().addQuery(Activities.QUERY_FOR_THINGS_POINTED_TO, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                Value actor = bindingSet.getValue("actor");
                Value indicated = bindingSet.getValue("indicated");
                System.out.println("got a result: " + actor + " pointed to " + indicated);
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
