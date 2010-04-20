package edu.rpi.tw.patadata;

import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.sail.SailConnection;
import edu.rpi.tw.patadata.ranking.WeightedVector;
import edu.rpi.tw.patadata.ranking.WeightedVectorApproximation;
import edu.rpi.tw.patadata.vocab.DBPediaProperties;

import java.util.Queue;
import java.util.LinkedList;
import java.util.Arrays;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 3:01:08 PM
 */
public class DBPediaSpreadVector implements WeightedVectorApproximation<Resource, PataException> {
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

    public int compute(int cycles) throws PataException {
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
                            final Queue<Resource> resources) throws PataException {
        if (INVERSE) {
             Handler<Resource, PataException> h = new Handler<Resource, PataException>() {
                public boolean handle(final Resource r) throws PataException {
                    result.addWeight(r, weight);
                    resources.offer(r);
                    return true;
                }
            };

            TraverserTools.traverseBackward(sc,
                    new KeepResourcesFilter(h),
                    resource,
                    RELATED_RESOURCE_PREDICATES);
        } else {
            Handler<Resource, PataException> h = new Handler<Resource, PataException>() {
                public boolean handle(final Resource r) throws PataException {
                    result.addWeight(r, weight);
                    resources.offer(r);
                    return true;
                }
            };

            TraverserTools.traverseForward(sc,
                    new KeepResourcesFilter(h),
                    resource,
                    RELATED_RESOURCE_PREDICATES);
        }
    }
}
