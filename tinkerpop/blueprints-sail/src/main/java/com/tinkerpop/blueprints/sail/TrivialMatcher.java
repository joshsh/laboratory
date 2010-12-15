package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Graph;

import java.util.Iterator;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TrivialMatcher extends Matcher {
    private final Graph graph;

    public TrivialMatcher(final Graph graph) {
        super(false, false, false, false);
        this.graph = graph;
    }

    @Override
    public Iterator<Edge> match(final String subject,
                                final String predicate,
                                final String object,
                                final String context) {
        return graph.getEdges().iterator();
    }
}
