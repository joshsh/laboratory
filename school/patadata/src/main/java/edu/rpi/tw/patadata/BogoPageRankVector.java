package edu.rpi.tw.patadata;

import edu.rpi.tw.patadata.ranking.WeightedVector;
import edu.rpi.tw.patadata.ranking.WeightedVectorApproximation;
import org.openrdf.model.Resource;
import org.openrdf.sail.SailConnection;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 7:10:39 PM
 */
public class BogoPageRankVector implements WeightedVectorApproximation<Resource, PataException> {
    private static final int CYCLES_PER_SEED = 100;
    
    private final WeightedVector<Resource> result;
    private final SailConnection sailConnection;

    public BogoPageRankVector(final SailConnection sailConnection) {
        this.sailConnection = sailConnection;
        result = new WeightedVector<Resource>();
    }

    public WeightedVector<Resource> currentResult() {
        return result.normalizedAsDist();
    }

    public int compute(int cycles) throws PataException {
        System.out.println("pagerank: " + cycles + " cycles");
        int s = cycles / CYCLES_PER_SEED;
        if (0 == s) {
            s = 1;
        }

        Resource[] seeds = new Resource[s];
        for (int i = 0; i < seeds.length; i++) {
            seeds[i] = TraverserTools.randomResource(sailConnection);
        }

        DBPediaSpreadVector p = new DBPediaSpreadVector(result, sailConnection, seeds);
        return p.compute(cycles);
    }
}
