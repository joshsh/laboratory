package net.fortytwo.sparqlosc;

import info.aduna.iteration.CloseableIteration;
import org.openrdf.model.Value;
import org.openrdf.query.Binding;
import org.openrdf.query.BindingSet;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.impl.MapBindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.sparql.SPARQLParser;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

/**
 * User: josh
 * Date: Jul 31, 2010
 * Time: 3:14:29 PM
 */
public class PollingSparqlOscService extends SparqlOscService {
    private static final String BASE_URI = "http://example.org/bogusBaseURI";

    private final Set<SubscriberWrapper> subscribers;
    private final Sail sail;

    public PollingSparqlOscService(final Sail sail,
                                   final long interval) {
        this.sail = sail;
        subscribers = new HashSet<SubscriberWrapper>();

        Timer timer = new Timer();
        TimerTask task = new TimerTask() {
            public void run() {
                try {
                    runQueries();
                } catch (Throwable t) {
                    System.err.println("error intercepted:");
                    t.printStackTrace(System.err);
                }
            }
        };
        timer.schedule(task, interval, interval);
    }

    public synchronized void subscribe(final SparqlOscSubscriber subscriber) {
        subscribers.add(new SubscriberWrapper(subscriber));
    }

    public synchronized void unsubscribe(final SparqlOscSubscriber subscriber) {
        // FIXME: this is odd
        subscribers.remove(new SubscriberWrapper(subscriber));
    }

    private synchronized Collection<SubscriberWrapper> getSubscribers() {
        Collection<SubscriberWrapper> buffer = new LinkedList<SubscriberWrapper>();
        buffer.addAll(subscribers);
        return buffer;
    }

    private void runQueries() throws SailException, MalformedQueryException, QueryEvaluationException, IOException {
        Collection<SubscriberWrapper> buffer = getSubscribers();

        SailConnection c = sail.getConnection();
        try {
            for (SubscriberWrapper w : buffer) {
                ParsedQuery query = w.getQuery();

                MapBindingSet bindings = new MapBindingSet();
                boolean includeInferred = false;
                CloseableIteration<? extends BindingSet, QueryEvaluationException> iter
                        = c.evaluate(query.getTupleExpr(), query.getDataset(), bindings, includeInferred);
                try {
                    while (iter.hasNext()) {
                        try {
                            BindingSet bs = iter.next();
                            if (w.addResult(bs)) {
                                this.handleSparqlResult(bs, w.getSubscriber());
                            }
                        } catch (SparqlOscMappingException e) {
                            System.err.println("mapping error ignored:");
                            e.printStackTrace(System.err);
                        }
                    }
                } finally {
                    iter.close();
                }
            }
        } finally {
            c.close();
        }
    }

    private static ParsedQuery parseQuery(final String query) throws MalformedQueryException {
        SPARQLParser parser = new SPARQLParser();
        return parser.parseQuery(query, BASE_URI);
    }

    private class SubscriberWrapper {
        private final SparqlOscSubscriber subscriber;
        private final Set<BindingSetWrapper> previousResults;
        private ParsedQuery query;

        public SubscriberWrapper(SparqlOscSubscriber subscriber) {
            this.subscriber = subscriber;
            this.previousResults = new HashSet<BindingSetWrapper>();
        }

        public SparqlOscSubscriber getSubscriber() {
            return subscriber;
        }

        public ParsedQuery getQuery() throws MalformedQueryException {
            if (null == query) {
                query = parseQuery(subscriber.getSparqlQuery());
            }

            return query;
        }

        public boolean equals(final Object other) {
            return (other instanceof SubscriberWrapper
                    && ((SubscriberWrapper) other).subscriber.equals(subscriber));
        }

        public int hashCode() {
            return subscriber.hashCode();
        }

        public boolean addResult(final BindingSet bs) {
            return previousResults.add(new BindingSetWrapper(bs));
        }
    }

    private class BindingSetWrapper {
        private final BindingSet set;

        public BindingSetWrapper(BindingSet set) {
            this.set = set;
        }

        public BindingSet getSet() {
            return set;
        }

        public boolean equals(final Object other) {
            if (!(other instanceof BindingSetWrapper)) {
                return false;
            } else {
                BindingSet s = ((BindingSetWrapper) other).set;

                if (s.size() != set.size()) {
                    return false;
                }

                for (String name : s.getBindingNames()) {
                    if (!valuesEqual(s.getValue(name), set.getValue(name))) {
                        return false;
                    }
                }

                return true;
            }
        }

        public int hashCode() {
            int c = 0;
            for (Binding b : set) {
                Value v = b.getValue();
                if (null == v) {
                    c += 7 * b.getName().hashCode();
                } else {
                    c += v.hashCode() * b.getName().hashCode();
                }
            }

            return c;
        }

        private boolean valuesEqual(final Value v1,
                                    final Value v2) {
            if (null == v1) {
                return null == v2;
            } else return null != v2 && v1.equals(v2);
        }
    }
}
