package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;

import java.util.Iterator;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Matcher {
    protected final boolean s, p, o, c;

    public Matcher(final boolean s,
                                final boolean p,
                                final boolean o,
                                final boolean c) {
        this.s = s;
        this.p = p;
        this.o = o;
        this.c = c;
    }

    public abstract Iterator<Edge> match(final String subject,
                                         final String predicate,
                                         final String object,
                                         final String context);

    public String toString() {
        StringBuilder sb = new StringBuilder("matcher[");
        if (s) {
            sb.append("s");
        }
        if (p) {
            sb.append("p");
        }
        if (o) {
            sb.append("o");
        }
        if (c) {
            sb.append("c");
        }
        sb.append("]");
        return sb.toString();
    }
}