package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Vertex;

import java.util.Iterator;

/**
 * A matcher which uses the vertex-edge structure of the graph to retrieve statements.  It does not require an edge index of any kind,
 * but it can only be applied to triple patterns in which the subject or object is specified
 * (which includes all patterns apart from "p", "c", and "pc").
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphBasedMatcher extends Matcher {
    private final Graph graph;

    /**
     * Create a new graph-based matcher with the given triple pattern.
     *
     * @param s whether the subject is specified
     * @param p whether the predicate is specified
     * @param o whether the object is specified
     * @param c whether the context is specified
     * @param graph the Blueprints property graph of the RDF store
     */
    public GraphBasedMatcher(final boolean s,
                             final boolean p,
                             final boolean o,
                             final boolean c,
                             final Graph graph) {
        super(s, p, o, c);
        this.graph = graph;
    }

    public Iterator<Edge> match(final String subject,
                                final String predicate,
                                final String object,
                                final String context) {
        //System.out.println("+ spoc: " + s + " " + p + " " + o + " " + c);
        //System.out.println("+ \ts: " + subject + ", p: " + predicate + ", o: " + object + ", c: " + context);

        if (s && o) {
            Vertex vs = graph.getVertex(subject);
            Vertex vo = graph.getVertex(object);

            if (null == vs || null == vo) {
                return new EmptyIterator<Edge>();
            } else {
                // TODO: use a simple heuristic (e.g. based on the value type of the vertices) to choose either subject or object.
                // Right now, we arbitrarily choose the subject as the starting point.
                return new FilteredIterator<Edge>(vs.getOutEdges().iterator(),
                        new FilteredIterator.Criterion<Edge>() {
                            public boolean fulfilledBy(final Edge edge) {
                                return edge.getInVertex().getId().equals(object)
                                        && (!p || edge.getLabel().equals(predicate))
                                        && (!c || edge.getProperty(BlueprintsSail.CONTEXT_PROP).equals(context));
                            }
                        });
            }
        } else if (s) {
            Vertex vs = graph.getVertex(subject);
            return null == vs
                    ? new EmptyIterator<Edge>()
                    : new FilteredIterator<Edge>(vs.getOutEdges().iterator(),
                    new FilteredIterator.Criterion<Edge>() {
                        public boolean fulfilledBy(final Edge edge) {
                            return (!p || edge.getLabel().equals(predicate))
                                    && (!c || edge.getProperty(BlueprintsSail.CONTEXT_PROP).equals(context));
                        }
                    });
        } else {
            Vertex vo = graph.getVertex(object);
            return null == vo
                    ? new EmptyIterator<Edge>()
                    : new FilteredIterator<Edge>(vo.getInEdges().iterator(),
                    new FilteredIterator.Criterion<Edge>() {
                        public boolean fulfilledBy(final Edge edge) {
                            return (!p || edge.getLabel().equals(predicate))
                                    && (!c || edge.getProperty(BlueprintsSail.CONTEXT_PROP).equals(context));
                        }
                    });
        }
    }

    private class EmptyIterator<T> implements Iterator<T> {
        public boolean hasNext() {
            return false;
        }

        public T next() {
            return null;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
