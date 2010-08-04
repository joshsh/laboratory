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
 * A simple SPARQL-OSC listener which polls a triple store periodically for new results for the SPARQL queries which have been registered.
 *
 * User: josh
 * Date: Jul 31, 2010
 * Time: 3:14:29 PM
 */
public class PollingSparqlOscListener extends SparqlOscListener {
    private static final String BASE_URI = "http://example.org/bogusBaseURI";

    private final Set<SubscriberWrapper> mappings;
    private final Sail sail;

    /**
     * @param sail a Sesame storage and inference layer (Sail) against which to evaluate SPARQL queries
     * @param interval how often the listener should poll the triple store for new results
     */
    public PollingSparqlOscListener(final Sail sail,
                                    final long interval) {
        this.sail = sail;
        mappings = new HashSet<SubscriberWrapper>();

        Timer timer = new Timer();
        TimerTask task = new TimerTask() {
            public void run() {
                try {
                    runQueries();
                } catch (Throwable t) {
                    LOGGER.severe("error intercepted:");
                    t.printStackTrace(System.err);
                }
            }
        };
        timer.schedule(task, interval, interval);
    }

    public synchronized void register(final SparqlOscMapping mapping) {
        mappings.add(new SubscriberWrapper(mapping));
    }

    public synchronized void unregister(final SparqlOscMapping mapping) {
        // FIXME: this is odd
        mappings.remove(new SubscriberWrapper(mapping));
    }

    private synchronized Collection<SubscriberWrapper> getMappingsBuffer() {
        Collection<SubscriberWrapper> buffer = new LinkedList<SubscriberWrapper>();
        buffer.addAll(mappings);
        return buffer;
    }

    private void runQueries() throws SailException, MalformedQueryException, QueryEvaluationException, IOException {
        Collection<SubscriberWrapper> buffer = getMappingsBuffer();

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
                            LOGGER.warning("mapping error ignored");
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
        private final SparqlOscMapping mapping;
        private final Set<BindingSetWrapper> previousResults;
        private ParsedQuery query;

        public SubscriberWrapper(SparqlOscMapping mapping) {
            this.mapping = mapping;
            this.previousResults = new HashSet<BindingSetWrapper>();
        }

        public SparqlOscMapping getSubscriber() {
            return mapping;
        }

        public ParsedQuery getQuery() throws MalformedQueryException {
            if (null == query) {
                //System.out.println("query: " + mapping.getSparqlQuery());
                query = parseQuery(mapping.getSparqlQuery());
            }

            return query;
        }

        public boolean equals(final Object other) {
            return (other instanceof SubscriberWrapper
                    && ((SubscriberWrapper) other).mapping.equals(mapping));
        }

        public int hashCode() {
            return mapping.hashCode();
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
