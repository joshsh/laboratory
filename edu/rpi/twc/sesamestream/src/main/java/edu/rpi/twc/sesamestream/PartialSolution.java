package edu.rpi.twc.sesamestream;

import java.util.Collection;
import java.util.Set;

/**
 * An intermediate result in the answering of a continuous query.
 * It contains zero or more already-completed bindings of variables to values
 * as well as one or more still-to-be-matched RDF triple patterns.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PartialSolution {

    private final Subscription subscription;

    private final Collection<TriplePattern> patterns;

    private final VarList bindings;

    /**
     * Constructs an initial partial solution with no bound variables.
     * This is the start state of a query, before any statements have been received.
     *
     * @param subscription an object containing the query and the handler for query results
     */
    public PartialSolution(final Subscription subscription) {
        this.subscription = subscription;

        bindings = VarList.NIL;

        // TODO: make this immutable, for performance sake
        patterns = subscription.getQuery().getTriplePatterns();
    }

    /**
     * Constructs an partial solution with the specified bindings and triple patterns.
     *
     * @param subscription an object containing the query and the handler for query results
     * @param patterns the still-to-be-matched RDF triple patterns
     * @param bindings the already-completed bindings of variables to values
     */
    public PartialSolution(final Subscription subscription,
                           final Set<TriplePattern> patterns,
                           final VarList bindings) {
        this.subscription = subscription;
        this.patterns = patterns;
        this.bindings = bindings;
    }

    /**
     * @return the already-completed bindings of variables to values of this partial solution
     */
    public VarList getBindings() {
        return bindings;
    }

    /**
     * @return the still-to-be-matched RDF triple patterns of this partial solution
     */
    public Collection<TriplePattern> getPatterns() {
        return patterns;
    }

    public Subscription getSubscription() {
        return subscription;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("PartialSolution(").append(bindings).append(", {");

        boolean first = true;
        for (TriplePattern t : patterns) {
            if (first) {
                first = false;
            } else {
                sb.append(",");
            }

            sb.append(t);
        }

        sb.append("})");
        return sb.toString();
    }
}
