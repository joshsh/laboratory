package edu.rpi.tw.patadata.ranking;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 3:35:45 PM
 */
public class ApproxIntersection<T, E extends Exception> extends ApproxFairOperation<T, E> {
    public ApproxIntersection(final WeightedVectorApproximation<T, E>... operands) {
        super(operands);
    }

    public WeightedVector<T> currentResult() {
        if (0 == operands.length) {
            return new WeightedVector<T>();
        } else {
            WeightedVector<T> cur = operands[0].currentResult();

            for (int i = 1; i < operands.length; i++) {
                cur = cur.multiplyByTransposeOf(operands[i].currentResult());
            }

            return cur;
        }
    }
}