package edu.rpi.tw.patadata.ranking;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 3:35:45 PM
 */
public class ApproxUnion<T, E extends Exception> extends ApproxFairOperation<T, E> {
    public ApproxUnion(final WeightedVectorApproximation<T, E>... operands) {
        super(operands);
    }

    public WeightedVector<T> currentResult() {
        WeightedVector<T> cur = new WeightedVector<T>();

        for (WeightedVectorApproximation<T, E> o : operands) {
            cur = cur.add(o.currentResult());
        }

        return cur;
    }
}
