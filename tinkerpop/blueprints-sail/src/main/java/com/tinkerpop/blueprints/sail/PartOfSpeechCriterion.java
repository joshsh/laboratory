package com.tinkerpop.blueprints.sail;

import com.tinkerpop.blueprints.pgm.Edge;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PartOfSpeechCriterion implements FilteredIterator.Criterion<Edge> {
    public enum PartOfSpeech {
        SUBJECT, PREDICATE, OBJECT, CONTEXT
    }

    private final PartOfSpeech partOfSpeech;
    private final String value;

    public PartOfSpeechCriterion(
            final PartOfSpeech partOfSpeech,
            final String value) {
        this.partOfSpeech = partOfSpeech;
        this.value = value;
    }

    public boolean passes(final Edge edge) {
        debugEdge(edge);
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

    public static void debugEdge(final Edge edge) {
        System.out.println("edge " + edge + ":");
        for (String key : edge.getPropertyKeys()) {
            System.out.println("\t" + key + ":\t'" + edge.getProperty(key) + "'");
        }
    }
}
