package edu.rpi.twc.sesamestream;

import org.openrdf.query.algebra.StatementPattern;
import org.openrdf.query.algebra.Var;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TriplePattern {
    private final Var subject;
    private final Var predicate;
    private final Var object;

    private final int hashCode;

    public TriplePattern(final Var subject,
                         final Var predicate,
                         final Var object) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;

        // TODO: this is not foolproof
        hashCode = (null == subject ? 1 : subject.hashCode())
                + (null == predicate ? 2 : 2 * predicate.hashCode())
                + (null == object ? 5 : 3 * object.hashCode());
    }

    public TriplePattern(final StatementPattern p) {
        this(p.getSubjectVar(), p.getPredicateVar(), p.getObjectVar());

        //System.out.println("statement pattern: " + p);
    }

    public Var getObject() {
        return object;
    }

    public Var getPredicate() {
        return predicate;
    }

    public Var getSubject() {
        return subject;
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    @Override
    public boolean equals(final Object other) {
        // TODO: test whether removing the "instanceof" improves performance
        return other instanceof TriplePattern && hashCode == ((TriplePattern) other).hashCode;
    }

    public String toString() {
        return "TriplePattern(" + toString(subject) + "," + toString(predicate) + "," + toString(object) + ")";
    }

    private String toString(final Var v) {
        if (v.hasValue()) {
            return v.getName() + ":" + v.getValue();
        } else {
            return v.getName() + "?";
        }
    }
}
