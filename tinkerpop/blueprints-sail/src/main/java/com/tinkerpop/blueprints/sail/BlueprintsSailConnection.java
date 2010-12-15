package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.TransactionalGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import info.aduna.iteration.CloseableIteration;
import net.fortytwo.sesametools.CompoundCloseableIteration;
import net.fortytwo.sesametools.SailConnectionTripleSource;
import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Namespace;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.NamespaceImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.query.Dataset;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.algebra.TupleExpr;
import org.openrdf.query.algebra.evaluation.TripleSource;
import org.openrdf.query.algebra.evaluation.impl.EvaluationStrategyImpl;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * User: josh
 * Date: Dec 11, 2010
 * Time: 1:00:12 PM
 */
public class BlueprintsSailConnection implements SailConnection {
    private static final Resource[] NULL_CONTEXT_ARRAY = {null};
    private static final String NULL_CONTEXT_NATIVE = "" + BlueprintsSail.NULL_CONTEXT_PREFIX;

    private final BlueprintsSail.Indexes indexes;

    private boolean open;

    public BlueprintsSailConnection(final BlueprintsSail.Indexes indexes) {
        this.indexes = indexes;

        if (indexes.manualTransactions) {
            ((TransactionalGraph) indexes.graph).startTransaction();
        }

        open = true;
    }

    public boolean isOpen() throws SailException {
        return open;
    }

    public void close() throws SailException {
        open = false;

        // Roll back any uncommitted operations.
        if (indexes.manualTransactions) {
            ((TransactionalGraph) indexes.graph).stopTransaction(TransactionalGraph.Conclusion.FAILURE);
        }
    }

    public CloseableIteration<? extends BindingSet, QueryEvaluationException> evaluate(final TupleExpr query,
                                                                                       final Dataset dataset,
                                                                                       final BindingSet bindings,
                                                                                       final boolean includeInferred) throws SailException {
        try {
            TripleSource tripleSource = new SailConnectionTripleSource(this,
                    indexes.valueFactory, includeInferred);
            EvaluationStrategyImpl strategy = new EvaluationStrategyImpl(
                    tripleSource, dataset);
            return strategy.evaluate(query, bindings);
        }
        catch (QueryEvaluationException e) {
            throw new SailException(e);
        }
    }

    public CloseableIteration<? extends Resource, SailException> getContextIDs() throws SailException {
        throw new UnsupportedOperationException("the getContextIDs operation is not yet supported");
    }

    public CloseableIteration<? extends Statement, SailException> getStatements(final Resource subject,
                                                                                final URI predicate,
                                                                                final Value object,
                                                                                final boolean includeInferred,
                                                                                final Resource... contexts) throws SailException {
        String s, p, o, c;

        int index = 0;

        if (null != subject) {
            index |= 0x1;
            s = resourceToNative(subject);
        } else {
            s = null;
        }

        if (null != predicate) {
            index |= 0x2;
            p = uriToNative(predicate);
        } else {
            p = null;
        }

        if (null != object) {
            index |= 0x4;
            o = valueToNative(object);
        } else {
            o = null;
        }

        if (0 == contexts.length) {
            return new EdgeIteration(indexes.matchers[index].match(s, p, o, null));
        } else {
            Collection<CloseableIteration<Statement, SailException>> iterations
                    = new LinkedList<CloseableIteration<Statement, SailException>>();

            // TODO: as an optimization, filter on multiple contexts simultaneously (when context is not used in the matcher), rather than trying each context consecutively.
            for (Resource context : contexts) {
                index |= 0x8;
                c = null == context ? NULL_CONTEXT_NATIVE : resourceToNative(context);

                TriplePatternMatcher m = indexes.matchers[index];
                iterations.add(
                        new EdgeIteration(m.match(s, p, o, c)));
            }

            return new CompoundCloseableIteration<Statement, SailException>(iterations);
        }
    }

    public long size(final Resource... contexts) throws SailException {
        if (0 == contexts.length) {
            return countIterator(indexes.matchers[0x0].match(null, null, null, null));
        } else {
            int count = 0;

            for (Resource context : contexts) {
                String c = null == context ? NULL_CONTEXT_NATIVE : resourceToNative(context);
                count += countIterator(indexes.matchers[0x8].match(null, null, null, c));
            }

            return count;
        }
    }

    private int countIterator(final Iterator i) {
        int count = 0;
        while (i.hasNext()) {
            count++;
            i.next();
        }
        return count;
    }

    public void commit() throws SailException {
        if (indexes.manualTransactions) {
            ((TransactionalGraph) indexes.graph).stopTransaction(TransactionalGraph.Conclusion.SUCCESS);
            ((TransactionalGraph) indexes.graph).startTransaction();
        }
    }

    public void rollback() throws SailException {
        if (indexes.manualTransactions) {
            ((TransactionalGraph) indexes.graph).stopTransaction(TransactionalGraph.Conclusion.FAILURE);
            ((TransactionalGraph) indexes.graph).startTransaction();
        }
    }

    // TODO: avoid duplicate statements
    public void addStatement(final Resource subject,
                             final URI predicate,
                             final Value object,
                             final Resource... contexts) throws SailException {
        for (Resource context : ((0 == contexts.length) ? NULL_CONTEXT_ARRAY : contexts)) {
            String s = resourceToNative(subject);
            String p = uriToNative(predicate);
            String o = valueToNative(object);
            String c = null == context ? NULL_CONTEXT_NATIVE : resourceToNative(context);

            //Vertex head = indexes.graph.addVertex(null);
            //Vertex tail = indexes.graph.addVertex(null);
            Vertex head = getOrCreateVertex(o);
            Vertex tail = getOrCreateVertex(s);
            Edge edge = indexes.graph.addEdge(null, head, tail, p);
            //edge.setProperty(BlueprintsSail.SUBJECT_PROP, s);
            //edge.setProperty(BlueprintsSail.PREDICATE_PROP, p);
            //edge.setProperty(BlueprintsSail.OBJECT_PROP, o);
            //edge.setProperty(BlueprintsSail.CONTEXT_PROP, c);

            for (TriplePatternMatcher m : indexes.indexingMatchers) {
                //System.out.println("\t\tindexing with: " + m);
                m.indexStatement(edge, s, p, o, c);
            }

            System.out.println("added (s: " + s + ", p: " + p + ", o: " + o + ", c: " + c + ")");
            System.out.print("\t--> ");
            PartOfSpeechCriterion.debugEdge(edge);
        }
    }

    private Vertex getOrCreateVertex(final String id) {
        Vertex v = indexes.graph.getVertex(id);
        if (null == v) {
            v = indexes.graph.addVertex(id);
        }
        return v;
    }

    public void removeStatements(final Resource subject,
                                 final URI predicate,
                                 final Value object,
                                 final Resource... contexts) throws SailException {
        Collection<Edge> edgesToRemove = new LinkedList<Edge>();

        String s, p, o, c;

        int index = 0;

        if (null != subject) {
            index |= 0x1;
            s = resourceToNative(subject);
        } else {
            s = null;
        }

        if (null != predicate) {
            index |= 0x2;
            p = uriToNative(predicate);
        } else {
            p = null;
        }

        if (null != object) {
            index |= 0x4;
            o = valueToNative(object);
        } else {
            o = null;
        }

        if (0 == contexts.length) {
            Iterator<Edge> i = indexes.matchers[index].match(s, p, o, null);
            while (i.hasNext()) {
                edgesToRemove.add(i.next());
            }
        } else {
            // TODO: as an optimization, filter on multiple contexts simultaneously (when context is not used in the matcher), rather than trying each context consecutively.
            for (Resource context : contexts) {
                index |= 0x8;
                c = null == context ? NULL_CONTEXT_NATIVE : resourceToNative(context);

                System.out.println("matcher: " + indexes.matchers[index]);
                Iterator<Edge> i = indexes.matchers[index].match(s, p, o, c);
                while (i.hasNext()) {
                    edgesToRemove.add(i.next());
                }
            }
        }

        for (Edge e : edgesToRemove) {
            System.out.println("removing this edge: " + e);
            indexes.graph.removeEdge(e);
        }
    }

    public void clear(final Resource... contexts) throws SailException {
        if (0 == contexts.length) {
            deleteEdgesInIterator(indexes.matchers[0x0].match(null, null, null, null));
        } else {
            for (Resource context : contexts) {
                String c = null == context ? NULL_CONTEXT_NATIVE : resourceToNative(context);
                deleteEdgesInIterator(indexes.matchers[0x8].match(null, null, null, c));
            }
        }
    }

    // TODO: beware of concurrent modification exceptions
    private void deleteEdgesInIterator(final Iterator<Edge> i) {
        while (i.hasNext()) {
            Edge e = i.next();
            i.remove();
            Vertex h = e.getInVertex();
            Vertex t = e.getOutVertex();
            indexes.graph.removeEdge(e);
            if (!h.getInEdges().iterator().hasNext() && !h.getOutEdges().iterator().hasNext()) {
                indexes.graph.removeVertex(h);
            }
            if (!t.getOutEdges().iterator().hasNext() && !t.getInEdges().iterator().hasNext()) {
                indexes.graph.removeVertex(t);
            }
        }
    }

    public CloseableIteration<? extends Namespace, SailException> getNamespaces() throws SailException {
        final Iterator<String> prefixes = indexes.namespaces.getPropertyKeys().iterator();

        return new CloseableIteration<Namespace, SailException>() {
            public void close() throws SailException {
                // Do nothing.
            }

            public boolean hasNext() throws SailException {
                return prefixes.hasNext();
            }

            public Namespace next() throws SailException {
                String prefix = prefixes.next();
                String uri = (String) indexes.namespaces.getProperty(prefix);
                return new NamespaceImpl(prefix, uri);
            }

            public void remove() throws SailException {
                throw new UnsupportedOperationException();
            }
        };
    }

    public String getNamespace(final String prefix) throws SailException {
        return (String) indexes.namespaces.getProperty(prefix);
    }

    public void setNamespace(final String prefix,
                             final String uri) throws SailException {
        indexes.namespaces.setProperty(prefix, uri);
    }

    public void removeNamespace(final String prefix) throws SailException {
        indexes.namespaces.removeProperty(prefix);
    }

    public void clearNamespaces() throws SailException {
        throw new UnsupportedOperationException("the clearNamespaces operation is not yet supported");
    }

    ////////////////////////////////////////////////////////////////////////////

    private class EdgeIteration implements CloseableIteration<Statement, SailException> {
        private final Iterator<Edge> iterator;

        public EdgeIteration(final Iterator<Edge> iterator) {
            this.iterator = iterator;
        }

        public void close() throws SailException {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public boolean hasNext() throws SailException {
            return iterator.hasNext();
        }

        // FIXME: handle null context case
        public Statement next() throws SailException {
            Edge e = iterator.next();
            Resource subject = (Resource) toSesame(((String) e.getProperty(BlueprintsSail.SUBJECT_PROP)).substring(1));
            URI predicate = (URI) toSesame(((String) e.getProperty(BlueprintsSail.PREDICATE_PROP)).substring(1));
            Value object = toSesame(((String) e.getProperty(BlueprintsSail.OBJECT_PROP)).substring(1));
            Resource context = (Resource) toSesame(((String) e.getProperty(BlueprintsSail.CONTEXT_PROP)).substring(1));

            return indexes.valueFactory.createStatement(subject, predicate, object, context);
        }

        public void remove() throws SailException {
            throw new UnsupportedOperationException();
        }
    }

    // value conversion ////////////////////////////////////////////////////////

    private Value toSesame(final String s) {
        int i;

        switch (s.charAt(0)) {
            case BlueprintsSail.URI_PREFIX:
                return indexes.valueFactory.createURI(s.substring(1));
            case BlueprintsSail.BLANK_NODE_PREFIX:
                return indexes.valueFactory.createBNode(s.substring(1));
            case BlueprintsSail.PLAIN_LITERAL_PREFIX:
                return indexes.valueFactory.createLiteral(s.substring(1));
            case BlueprintsSail.TYPED_LITERAL_PREFIX:
                i = s.indexOf(BlueprintsSail.SEPARATOR);
                return indexes.valueFactory.createLiteral(s.substring(i + 1), indexes.valueFactory.createURI(s.substring(1, i)));
            case BlueprintsSail.LANGUAGE_TAG_LITERAL_PREFIX:
                i = s.indexOf(BlueprintsSail.SEPARATOR);
                return indexes.valueFactory.createLiteral(s.substring(i + 1), s.substring(1, i));
            case BlueprintsSail.NULL_CONTEXT_PREFIX:
                return null;
            default:
                throw new IllegalStateException();
        }
    }

    private String valueToNative(final Value value) {
        if (value instanceof Resource) {
            return resourceToNative((Resource) value);
        } else if (value instanceof Literal) {
            return literalToNative((Literal) value);
        } else {
            throw new IllegalStateException("value has unfamiliar type: " + value);
        }
    }

    private String resourceToNative(final Resource value) {
        if (value instanceof URI) {
            return uriToNative((URI) value);
        } else if (value instanceof BNode) {
            return bnodeToNative((BNode) value);
        } else {
            throw new IllegalStateException("resource has unfamiliar type: " + value);
        }
    }

    private String uriToNative(final URI value) {
        return BlueprintsSail.URI_PREFIX + value.toString();
    }

    private String bnodeToNative(final BNode value) {
        return BlueprintsSail.BLANK_NODE_PREFIX + value.getID();
    }

    private String literalToNative(final Literal literal) {
        URI datatype = literal.getDatatype();

        if (null == datatype) {
            String language = literal.getLanguage();

            if (null == language) {
                return BlueprintsSail.PLAIN_LITERAL_PREFIX + literal.getLabel();
            } else {
                return BlueprintsSail.LANGUAGE_TAG_LITERAL_PREFIX + language
                        + BlueprintsSail.SEPARATOR + literal.getLabel();
            }
        } else {
            // FIXME
            return "" + BlueprintsSail.TYPED_LITERAL_PREFIX + datatype
                    + BlueprintsSail.SEPARATOR + literal.getLabel();
        }
    }
}
