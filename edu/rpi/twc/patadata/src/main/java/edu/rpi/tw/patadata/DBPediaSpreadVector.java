package edu.rpi.tw.patadata;

import edu.rpi.tw.patadata.vocab.DBPediaProperties;
import net.fortytwo.flow.rdf.ranking.Handler;
import net.fortytwo.flow.rdf.ranking.HandlerException;
import net.fortytwo.flow.rdf.ranking.KeepResourcesFilter;
import net.fortytwo.flow.rdf.ranking.Ranking;
import net.fortytwo.flow.rdf.ranking.WeightedVector;
import net.fortytwo.flow.rdf.ranking.WeightedVectorApproximation;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.sail.SailConnection;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

public class DBPediaSpreadVector implements WeightedVectorApproximation<Resource, HandlerException> {
//    private static final double DECAY = 0.1;
    private static final double DECAY = 0.85;

    // TODO: temporary
    private static final boolean INVERSE = false;

    public static final URI[] RELATED_RESOURCE_PREDICATES = {
            //RDF.TYPE,
            //OWL.SAMEAS,
            DBPediaProperties.WIKILINK,
    };

    private final WeightedVector<Resource> vector;
    private Queue<Resource> curGen;
    private Queue<Resource> nextGen;
    private double weight;
    private final SailConnection sailConnection;
    private final Resource[] seeds;

    public DBPediaSpreadVector(final WeightedVector<Resource> vector,
                               final SailConnection sailConnection,
                               final Resource... seeds) {
        this.vector = vector;
        this.sailConnection = sailConnection;
        this.seeds = seeds;

        curGen = new LinkedList<Resource>();
        curGen.addAll(Arrays.asList(seeds));

        nextGen = new LinkedList<Resource>();

        weight = 1.0;
    }

    public DBPediaSpreadVector(final SailConnection sailConnection,
                               final Resource... seeds) {
        this.sailConnection = sailConnection;
        this.vector = new WeightedVector<Resource>();
        this.seeds = seeds;

        curGen = new LinkedList<Resource>();
        curGen.addAll(Arrays.asList(seeds));

        nextGen = new LinkedList<Resource>();

        weight = 1.0;
    }

    public WeightedVector<Resource> currentResult() {
        // Note: don't normalize the intermediate vector, in case we continue extending it.
        WeightedVector<Resource> normed = new WeightedVector<Resource>(vector);

        for (Resource s : seeds) {
            normed.setWeight(s, 0.0);
        }

        normed.normalizeAsDist();

        return normed;
    }

    public int compute(int cycles) throws HandlerException {
        System.out.println("dbpedia spreader: " + cycles + " cycles over " + seeds.length + " seeds: " + seeds[0] + "...");
        for (int i = 0; i < cycles; i++) {
            if (0 == curGen.size()) {
                if (0 == nextGen.size()) {
                    return 0;
                } else {
                    System.out.println("weight " + weight + " --> " + weight * DECAY);
                    weight *= DECAY;
                    Queue<Resource> tmp = curGen;
                    curGen = nextGen;
                    nextGen = tmp;
                }
            } else {
                Resource r = curGen.remove();

                stepRelated(sailConnection, r, weight, vector, nextGen);
            }
        }

        return cycles;
    }

    public void stepRelated(final SailConnection sc,
                            final Resource resource,
                            final double weight,
                            final WeightedVector<Resource> result,
                            final Queue<Resource> resources) throws HandlerException {
        if (INVERSE) {
             Handler<Resource, HandlerException> h = new Handler<Resource, HandlerException>() {
                public boolean handle(final Resource r) throws HandlerException {
                    result.addWeight(r, weight);
                    resources.offer(r);
                    return true;
                }
            };

            Ranking.traverseBackward(sc,
                    new KeepResourcesFilter(h),
                    resource,
                    RELATED_RESOURCE_PREDICATES);
        } else {
            Handler<Resource, HandlerException> h = new Handler<Resource, HandlerException>() {
                public boolean handle(final Resource r) throws HandlerException {
                    result.addWeight(r, weight);
                    resources.offer(r);
                    return true;
                }
            };

            Ranking.traverseForward(sc,
                    new KeepResourcesFilter(h),
                    resource,
                    RELATED_RESOURCE_PREDICATES);
        }
    }
}
