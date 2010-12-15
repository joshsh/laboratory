package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Index;

import java.util.Iterator;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TrivialTriplePatternMatcher extends TriplePatternMatcher {
    private final Graph graph;

    public TrivialTriplePatternMatcher(final Index<Edge> edges,
                                       final Graph graph) {
        super(edges, false, false, false, false);
        this.graph = graph;
    }

    @Override
    public void indexStatement(final Edge edge,
                               final String subject,
                               final String predicate,
                               final String object,
                               final String context) {
        // Do nothing.
    }

    @Override
    public Iterator<Edge> match(final String subject,
                                final String predicate,
                                final String object,
                                final String context) {
        return graph.getEdges().iterator();
    }
}
