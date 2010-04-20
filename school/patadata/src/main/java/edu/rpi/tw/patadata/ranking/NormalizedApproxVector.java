package edu.rpi.tw.patadata.ranking;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 6:52:13 PM
 */
public class NormalizedApproxVector<T, E extends Exception> implements WeightedVectorApproximation<T, E> {
    private final WeightedVectorApproximation<T, E> innerVector;

    public NormalizedApproxVector(final WeightedVectorApproximation<T, E> innerVector) {
        this.innerVector = innerVector;
    }

    public WeightedVector<T> currentResult() {
        return innerVector.currentResult().normalizedAsDist();
    }

    public int compute(int cycles) throws E {
        return innerVector.compute(cycles);
    }
}
