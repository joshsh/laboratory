package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Index;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Note: not appropriate for the "all wildcards" case
 * <p/>
 * User: josh
 * Date: Dec 13, 2010
 * Time: 2:11:35 PM
 */
class TriplePatternMatcher {
    private final String propertyName;
    private final boolean s, p, o, c;
    private final Index<Edge> edges;

    public TriplePatternMatcher(final Index<Edge> edges,
                                final boolean s,
                                final boolean p,
                                final boolean o,
                                final boolean c) {
        this.edges = edges;

        this.s = s;
        this.p = p;
        this.o = o;
        this.c = c;

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
            criteria.add(new PartOfSpeechCriterion(PartOfSpeechCriterion.PartOfSpeech.CONTEXT, context));
        }
        if (s) {
            sb.append(BlueprintsSail.SEPARATOR).append(subject);
        } else if (null != subject) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeechCriterion.PartOfSpeech.SUBJECT, subject));
        }

        if (p) {
            sb.append(BlueprintsSail.SEPARATOR).append(predicate);
        } else if (null != predicate) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeechCriterion.PartOfSpeech.PREDICATE, predicate));
        }

        if (o) {
            sb.append(BlueprintsSail.SEPARATOR).append(object);
        } else if (null != object) {
            criteria.add(new PartOfSpeechCriterion(PartOfSpeechCriterion.PartOfSpeech.OBJECT, object));
        }

        System.out.println("spoc: " + s + " " + p + " " + o + " " + c);
        System.out.println("\tsubject: " + subject + ", predicate: " + predicate + ", object: " + object + ", context: " + context);

        {
            Iterator<Edge> results = edges.get(propertyName, sb.toString()).iterator();
            int count = 0;
            while (results.hasNext()) {
                results.next();
                count++;
            }
            System.out.println("\t" + count + " results for property " + propertyName + ", value: '" + sb.toString() + "'.");
        }

        Iterator<Edge> results = edges.get(propertyName, sb.toString()).iterator();

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
        edge.setProperty(propertyName, sb.toString());
    }

    // TODO: unindexStatement

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
