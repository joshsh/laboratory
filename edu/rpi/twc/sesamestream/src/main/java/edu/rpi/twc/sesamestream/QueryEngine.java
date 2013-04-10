package edu.rpi.twc.sesamestream;

import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.algebra.TupleExpr;
import org.openrdf.query.algebra.Var;
import org.openrdf.query.impl.MapBindingSet;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 *         <p/>
 *         Current assumptions:
 *         1) only simple, conjunctive SELECT queries (no filters, etc.)
 *         2) no named graphs
 *         3) no duplicate queries
 *         4) no duplicate statements
 *         5) no duplicate triple patterns in a query
 *         6) single query at a time
 *         7) no combo of queries and statements such that multiple triple patterns match the same statement
 *         8 queries are only added, never removed
 *         9) statements are only added, never removed
 *         10) a statement will never match more than one triple pattern, per query, at a time
 */
public class QueryEngine {
    private static final boolean COMPACT_LOG_FORMAT = true;

    //private final Map<TriplePattern, Collection<PartialSolution>> oldIndex;
    private final TripleIndex index;

    private final List<PartialSolution> intermediateResultBuffer = new LinkedList<PartialSolution>();

    // TODO: this is a bit of a hack, and a waste of space
    private final Map<TriplePattern, TriplePattern> uniquePatterns;
    private final TriplePatternDeduplicator deduplicator;

    private final SolutionBinder binder = new SolutionBinder() {
        public void bind(final PartialSolution ps,
                         final TriplePattern p,
                         final VarList l) {
            increment(countBindingOps, false);

            bindSolution(ps, p, l);
        }
    };

    private boolean logHasChanged = false;

    private final Counter
            countQueries = new Counter(),
            countTriplePatterns = new Counter(),
            countStatements = new Counter(),
            countIntermediateResults = new Counter(),
            countSolutions = new Counter(),
            countIndexTriplePatternOps = new Counter(),
            countBindingOps = new Counter(),
            countReplaceOps = new Counter();

    public QueryEngine() {
        index = new TripleIndex();
        //oldIndex = new HashMap<TriplePattern, Collection<PartialSolution>>();
        uniquePatterns = new HashMap<TriplePattern, TriplePattern>();
        deduplicator = new TriplePatternDeduplicator();
        clear();
    }

    /**
     * Removes all queries, statements, and intermediate results.
     */
    public void clear() {
        // TODO: index.clear()
        //oldIndex.clear();
        uniquePatterns.clear();

        countQueries.reset();
        countTriplePatterns.reset();
        countStatements.reset();
        countIntermediateResults.reset();
        countSolutions.reset();
        countIndexTriplePatternOps.reset();
        countBindingOps.reset();
        countReplaceOps.reset();

        logHeader();
    }

    private void addIntermediateResult(final PartialSolution q) {
        increment(countIntermediateResults, true);
        //System.out.println("intermediate result:\t" + q);

        intermediateResultBuffer.add(q);
    }

    private void flushIntermediateResults() {
        for (PartialSolution q : intermediateResultBuffer) {
            for (TriplePattern tp : q.getGraphPattern()) {
                indexTriplePattern(tp, q);
            }
        }
        intermediateResultBuffer.clear();
    }

    /**
     * Adds a new query (subscription) to this query engine
     *
     * @param t the query to add
     * @param h a handler for eventual results of the query
     * @throws Query.IncompatibleQueryException
     *          if the syntax of the query is not supported by this engine
     */
    public void addQuery(final TupleExpr t,
                         final BindingSetHandler h) throws Query.IncompatibleQueryException {
        increment(countQueries, true);
        //System.out.println("query:\t" + t);

        Query q = new Query(t, deduplicator);

        Subscription s = new Subscription(q, h);

        PartialSolution query = new PartialSolution(s);
        addIntermediateResult(query);
        flushIntermediateResults();

        logEntry();
    }

    private VarList toVarList(final Statement s) {
        VarList l = VarList.NIL;

        l = new VarList(null, s.getObject(), l);
        l = new VarList(null, s.getPredicate(), l);
        l = new VarList(null, s.getSubject(), l);
        return l;
    }

    /**
     * Adds a new statement to this query engine.
     * Depending on the queries registered with this engine,
     * the statement will either be discarded as irrelevant to the queries,
     * trigger the creation of intermediate query results which are stored in anticipation of further statements,
     * or trigger the production final query results
     *
     * @param s the statement to add
     */
    public void addStatement(final Statement s) {
        increment(countStatements, false);
        //System.out.println("statement:\t" + s);

        index.match(toVarList(s), s, binder);

        /*
        // TODO: replace this linear search with something more efficient
        for (TriplePattern p : oldIndex.keySet()) {
            VarList l = applyTo(p, s);

            if (null != l) {
                for (PartialSolution q : oldIndex.get(p)) {
                    //System.out.println("handling match " + bindings + " of " + p + " in " + q);
                    bind(q, p, l);
                }
            }
        }
        */

        flushIntermediateResults();

        logEntry();
    }

    private VarList toVarList(TriplePattern p) {
        VarList l = VarList.NIL;
        l = new VarList(p.getObject().getName(), p.getObject().getValue(), l);
        l = new VarList(p.getPredicate().getName(), p.getPredicate().getValue(), l);
        l = new VarList(p.getSubject().getName(), p.getSubject().getValue(), l);
        return l;
    }

    private void indexTriplePattern(final TriplePattern p,
                                    final PartialSolution q) {
        increment(countIndexTriplePatternOps, false);

        //System.out.println("binding...\t" + p + " -- " + q);

        VarList l = toVarList(p);
        index.index(l, q);

        /*
        Collection<PartialSolution> queries = oldIndex.get(p);
        if (null == queries) {
            increment(countTriplePatterns, true);
            //System.out.println("triple pattern:\t" + p);

            queries = new LinkedList<PartialSolution>();
            oldIndex.put(p, queries);

            // This is necessarily a unique triple pattern
            uniquePatterns.put(p, p);
        }

        queries.add(q);

        //for (TriplePattern t : index.keySet()) {
        //    System.out.println("\t" + index.get(t).size() + " -- " + t);
        //}
        */
    }

    public void bindSolution(final PartialSolution r,
                     final TriplePattern satisfiedPattern,
                     final VarList newBindings) {
        //System.out.println("triple pattern satisfied: " + satisfiedPattern + " with bindings " + newBindings);
        if (1 == r.getGraphPattern().size()) {
            //System.out.println("producing solution: " + newBindings);
            produceSolution(r, r.getBindings().prepend(newBindings));
        } else {
            //System.out.println("creating new query");
            Set<TriplePattern> nextPatterns = new HashSet<TriplePattern>();

            VarList nextBindings = r.getBindings().prepend(newBindings);

            for (TriplePattern t : r.getGraphPattern()) {
                // Note: comparison with == is appropriate here thanks to deduplication of triple patterns
                if (t != satisfiedPattern) {
                    TriplePattern p = replace(t, newBindings);

                    if (null == p) {
                        nextPatterns.add(t);
                    } else {
                        nextPatterns.add(deduplicator.deduplicate(p));
                    }
                }
            }

            PartialSolution nextQuery
                    = new PartialSolution(r.getSubscription(), nextPatterns, nextBindings);

            addIntermediateResult(nextQuery);
        }
    }

    // Note: this operation doesn't need to be counted; it happens exactly once for each solution (which are counted)
    private void produceSolution(final PartialSolution r,
                                 final VarList nextBindings) {
        MapBindingSet b = new MapBindingSet();
        VarList cur = nextBindings;
        while (!cur.isNil()) {
            if (r.getSubscription().getQuery().getBindingNames().contains(cur.getName())) {
                //System.out.println("\t:" + cur);
                String n = r.getSubscription().getQuery().getExtendedBindingNames().get(cur.getName());
                if (null == n) {
                    n = cur.getName();
                }
                b.addBinding(n, cur.getValue());
            }
            cur = cur.getNext();
        }

        handleSolution(r.getSubscription().getHandler(), b);
    }

    // TODO: currently not efficient
    private TriplePattern replace(final TriplePattern p,
                                  final VarList bindings) {
        increment(countReplaceOps, false);

        Var newSubject = p.getSubject();
        Var newPredicate = p.getPredicate();
        Var newObject = p.getObject();
        boolean changed = false;

        VarList cur = bindings;
        while (null != cur) {
            if (!p.getSubject().hasValue() && p.getSubject().getName().equals(cur.getName())) {
                newSubject = cur.asVar();
                changed = true;
            }

            if (!p.getPredicate().hasValue() && p.getPredicate().getName().equals(cur.getName())) {
                newPredicate = cur.asVar();
                changed = true;
            }

            if (!p.getObject().hasValue() && p.getObject().getName().equals(cur.getName())) {
                newObject = cur.asVar();
                changed = true;
            }

            cur = cur.getNext();
        }

        return changed
                ? new TriplePattern(newSubject, newPredicate, newObject)
                : null;
    }

    private void increment(final Counter counter,
                           final boolean logChange) {
        if (SesameStream.PERFORMANCE_METRICS) {
            counter.increment();
            if (logChange) {
                logHasChanged = true;
            }
        }
    }

    private void logHeader() {
        if (SesameStream.PERFORMANCE_METRICS) {
            System.out.println("LOG\ttime,queries,statements,patterns,intermediate,solutions,indexTriplePatternOps,bindingOps,replaceOps");
        }
    }

    private void logEntry() {
        if (SesameStream.PERFORMANCE_METRICS) {
            if (!COMPACT_LOG_FORMAT || logHasChanged) {
                System.out.println("LOG\t" + System.currentTimeMillis()
                        + "," + countQueries.getCount()
                        + "," + countStatements.getCount()
                        + "," + countTriplePatterns.getCount()
                        + "," + countIntermediateResults.getCount()
                        + "," + countSolutions.getCount()
                        + "," + countIndexTriplePatternOps.getCount()
                        + "," + countBindingOps.getCount()
                        + "," + countReplaceOps.getCount());

                logHasChanged = false;
            }
        }
    }

    //*
    private static String toString(final BindingSet b) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (String n : b.getBindingNames()) {
            if (first) {
                first = false;
            } else {
                sb.append(", ");
            }

            sb.append(n).append(":").append(b.getValue(n));
        }

        return sb.toString();
    }//*/

    private void handleSolution(final BindingSetHandler handler,
                                final BindingSet solution) {
        increment(countSolutions, true);
        System.out.println("SOLUTION:\t" + QueryEngine.toString(solution));

        handler.handle(solution);
    }

    // Ensures that triple patterns created by replacement are not duplicate objects
    public class TriplePatternDeduplicator {
        public TriplePattern deduplicate(final TriplePattern t) {
            TriplePattern t2 = uniquePatterns.get(t);
            if (null == t2) {
                increment(countTriplePatterns, true);
                return t;
            } else {
                return t2;
            }
        }
    }

    private static class Counter {
        private long count = 0;

        public void increment() {
            count++;
        }

        public void reset() {
            count = 0;
        }

        public long getCount() {
            return count;
        }
    }
}
