package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Index;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class IndexingMatcher extends Matcher {
    public enum PartOfSpeech {
        SUBJECT, PREDICATE, OBJECT, CONTEXT
    }

    private final String propertyName;
    private final Index<Edge> edges;

    public IndexingMatcher(final Index<Edge> edges,
                           final boolean s,
                           final boolean p,
                           final boolean o,
                           final boolean c) {
        super(s, p, o, c);

        this.edges = edges;

        StringBuilder sb = new StringBuilder();
        if (c) {
            sb.append("c");
        }
        if (s) {
            sb.append("s");
        }
        if (p) {
            sb.append("p");
        }
        if (o) {
            sb.append("o");
        }
        propertyName = sb.toString();
    }

    public Iterator<Edge> match(final String subject,
                                final String predicate,
                                final String object,
                                final String context) {
        // FIXME: the temporary linked list is a little wasty
        List<PartOfSpeechCriterion> criteria = new LinkedList<PartOfSpeechCriterion>();

        StringBuilder sb = new StringBuilder();

        if (c) {
            sb.append(BlueprintsSail.SEPARATOR).append(context);
        } else if (null != context) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeech.CONTEXT, context));
        }
        if (s) {
            sb.append(BlueprintsSail.SEPARATOR).append(subject);
        } else if (null != subject) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeech.SUBJECT, subject));
        }

        if (p) {
            sb.append(BlueprintsSail.SEPARATOR).append(predicate);
        } else if (null != predicate) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeech.PREDICATE, predicate));
        }

        if (o) {
            sb.append(BlueprintsSail.SEPARATOR).append(object);
        } else if (null != object) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeech.OBJECT, object));
        }

        System.out.println("spoc: " + s + " " + p + " " + o + " " + c);
        System.out.println("\ts: " + subject + ", p: " + predicate + ", o: " + object + ", c: " + context);

        {
            Iterator<Edge> results = edges.get(propertyName, sb.toString().substring(1)).iterator();
            int count = 0;
            while (results.hasNext()) {
                results.next();
                count++;
            }
            System.out.println("\t" + count + " results for property " + propertyName + ", value: '" + sb.toString() + "'.");
        }

        Iterator<Edge> results = edges.get(propertyName, sb.toString().substring(1)).iterator();

        for (PartOfSpeechCriterion m : criteria) {
            results = new FilteredIterator<Edge>(results, m);
        }

        return results;
    }

    public void indexStatement(final Edge edge,
                               final String subject,
                               final String predicate,
                               final String object,
                               final String context) {
        StringBuilder sb = new StringBuilder();

        if (c) {
            sb.append(BlueprintsSail.SEPARATOR).append(context);
        }

        if (s) {
            sb.append(BlueprintsSail.SEPARATOR).append(subject);
        }

        if (p) {
            sb.append(BlueprintsSail.SEPARATOR).append(predicate);
        }

        if (o) {
            sb.append(BlueprintsSail.SEPARATOR).append(object);
        }

        //edges.put(propertyName, sb.toString(), edge);
        edge.setProperty(propertyName, sb.toString().substring(1));
    }

    // TODO: unindexStatement

    private class PartOfSpeechCriterion implements FilteredIterator.Criterion<Edge> {
        private final PartOfSpeech partOfSpeech;
        private final String value;

        public PartOfSpeechCriterion(
                final PartOfSpeech partOfSpeech,
                final String value) {
            this.partOfSpeech = partOfSpeech;
            this.value = value;
        }

        public boolean passes(final Edge edge) {
            BlueprintsSail.debugEdge(edge);
            System.out.println("pos: " + partOfSpeech + ", value: " + value);

            switch (partOfSpeech) {
                case CONTEXT:
                    return value.equals(edge.getProperty(BlueprintsSail.CONTEXT_PROP));
                case OBJECT:
                    return value.equals(edge.getProperty(BlueprintsSail.OBJECT_PROP));
                case PREDICATE:
                    return value.equals(edge.getProperty(BlueprintsSail.PREDICATE_PROP));
                case SUBJECT:
                    return value.equals(edge.getProperty(BlueprintsSail.SUBJECT_PROP));
                default:
                    throw new IllegalStateException();
            }
        }
    }
}
