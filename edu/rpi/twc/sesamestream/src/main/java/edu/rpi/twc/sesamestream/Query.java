package edu.rpi.twc.sesamestream;

import org.openrdf.query.algebra.Extension;
import org.openrdf.query.algebra.ExtensionElem;
import org.openrdf.query.algebra.Join;
import org.openrdf.query.algebra.Projection;
import org.openrdf.query.algebra.ProjectionElem;
import org.openrdf.query.algebra.ProjectionElemList;
import org.openrdf.query.algebra.QueryModelNode;
import org.openrdf.query.algebra.StatementPattern;
import org.openrdf.query.algebra.TupleExpr;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Query {
    private final Set<String> bindingNames;
    private final Map<String, String> extendedBindingNames;
    private final Set<TriplePattern> graphPattern;

    public Query(final TupleExpr e,
                 final QueryEngine.TriplePatternDeduplicator deduplicator) throws IncompatibleQueryException {
        bindingNames = new HashSet<String>();
        extendedBindingNames = new HashMap<String, String>();

        graphPattern = new HashSet<TriplePattern>();

        List<QueryModelNode> l = visit(e);
        if (l.size() != 1) {
            throw new IncompatibleQueryException();
        }
        Projection p = (Projection) l.iterator().next();
        for (ProjectionElem el : p.getProjectionElemList().getElements()) {
            extendedBindingNames.put(el.getSourceName(), el.getTargetName());
        }

        // TODO: eliminate redundant patterns
        for (StatementPattern pat : findStatementPatterns(p)) {
            graphPattern.add(deduplicator.deduplicate(new TriplePattern(pat)));
        }
    }

    public Set<TriplePattern> getGraphPattern() {
        return graphPattern;
    }

    public Map<String, String> getExtendedBindingNames() {
        return extendedBindingNames;
    }

    private Collection<StatementPattern> findStatementPatterns(final Join j) throws IncompatibleQueryException {
        Collection<StatementPattern> p = new LinkedList<StatementPattern>();

        for (QueryModelNode n : visitChildren(j)) {
            if (n instanceof StatementPattern) {
                p.add((StatementPattern) n);
            } else if (n instanceof Join) {
                p.addAll(findStatementPatterns((Join) n));
            } else {
                throw new IncompatibleQueryException("unexpected node: " + n);
            }
        }

        return p;
    }

    private Collection<StatementPattern> findStatementPatterns(final Projection p) throws IncompatibleQueryException {
        List<QueryModelNode> l = visitChildren(p);

        Extension ext = null;

        for (QueryModelNode n : l) {
            if (n instanceof Extension) {
                ext = (Extension) n;
            } else if (n instanceof ProjectionElemList) {
                ProjectionElemList pl = (ProjectionElemList) n;
                for (ProjectionElem pe : pl.getElements()) {
                    bindingNames.add(pe.getSourceName());
                    extendedBindingNames.put(pe.getSourceName(), pe.getTargetName());
                }
            }
        }

        if (null != ext) {
            l = visitChildren(ext);
        }

        for (QueryModelNode n : l) {
            if (n instanceof Join) {
                Join j = (Join) n;
                if (j.hasSubSelectInRightArg()) {
                    throw new IncompatibleQueryException();
                }

                return findStatementPatterns(j);
            } else if (n instanceof StatementPattern) {
                Collection<StatementPattern> c = new LinkedList<StatementPattern>();
                c.add((StatementPattern) n);
                return c;
            } else if (n instanceof ProjectionElemList) {
            } else if (n instanceof ExtensionElem) {
            } else {
                throw new IncompatibleQueryException("unexpected type: " + n.getClass());
            }
        }

        throw new IncompatibleQueryException();
    }

    private List<QueryModelNode> visit(final QueryModelNode node) {
        //System.out.println("### visit");
        List<QueryModelNode> visited = new LinkedList<QueryModelNode>();
        SimpleVisitor v = new SimpleVisitor(visited);

        try {
            node.visit(v);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        //for (QueryModelNode n : visited) {
        //    System.out.println("node: " + n);
        //}

        return visited;
    }

    private List<QueryModelNode> visitChildren(final QueryModelNode node) {
        //System.out.println("### visitChildren");
        List<QueryModelNode> visited = new LinkedList<QueryModelNode>();
        SimpleVisitor v = new SimpleVisitor(visited);

        try {
            node.visitChildren(v);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        //for (QueryModelNode n : visited) {
        //    System.out.println("node: " + n);
        //}

        return visited;
    }

    public Set<String> getBindingNames() {
        return bindingNames;
    }

    public static class IncompatibleQueryException extends Exception {
        public IncompatibleQueryException() {
            super();
        }

        public IncompatibleQueryException(final String message) {
            super(message);
        }
    }
}
