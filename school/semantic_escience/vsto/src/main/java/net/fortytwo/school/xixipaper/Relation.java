package net.fortytwo.school.xixipaper;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Dec 12, 2008
 * Time: 9:19:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class Relation {
    // FIXME: an adjacency matrix is convenient for small examples, but not scalable.
    private boolean[][] adjacencyMatrix;
    
    private int domainSize, rangeSize;
    private int size;

    public Relation(final int domainSize, final int rangeSize) {
        this.domainSize = domainSize;
        this.rangeSize = rangeSize;

        adjacencyMatrix = new boolean[domainSize][rangeSize];

        for (int i = 0; i < domainSize; i++) {
            for (int j = 0; j < rangeSize; j++) {
                adjacencyMatrix[i][j] = false;
            }
        }

        size = 0;
    }

    public Relation addPair(final int d, final int r) {
        if (!adjacencyMatrix[d][r]) {
            size++;
        }

        adjacencyMatrix[d][r] = true;

        return this;
    }

    // TODO: is there any point in finding the distribution with respect to the domain?
    public ProbabilityDistribution distributionInTermsOfRange() throws Exception {
        double [] probs = new double[rangeSize];

        for (int j = 0; j < rangeSize; j++) {
            int sum = 0;

            for (int i = 0; i < domainSize; i++) {
                if (adjacencyMatrix[i][j]) {
                    sum++;
                }
            }

            probs[j] = ((double) sum) / ((double) size);
        }

        return new ProbabilityDistribution(probs);
    }

    public void addPairs(final Relation other) {
        for (int i = 0; i < other.domainSize; i++) {
            for (int j = 0; j < other.rangeSize; j++) {
                if (other.adjacencyMatrix[i][j]) {
                    addPair(i, j);
                }
            }
        }
    }

    public Relation adjustedRelation() {
        int unusedDomain = domainSize;

        for (int i = 0; i < domainSize; i++) {
            for (int j = 0; j < rangeSize; j++) {
                if (adjacencyMatrix[i][j]) {
                    unusedDomain--;
                    break;
                }
            }
        }

        Relation newRelation = new Relation(domainSize, rangeSize + unusedDomain);
        newRelation.addPairs(this);
        int r = rangeSize;
        for (int i = 0; i < domainSize; i++) {
            boolean used = false;

            for (int j = 0; j < rangeSize; j++) {
                if (adjacencyMatrix[i][j]) {
                    used = true;
                    break;
                }
            }

            if (!used) {
                newRelation.addPair(i, r);
                r++;
            }
        }

        return newRelation;
    }
}
