package edu.rpi.tw.patadata.ranking;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 1:35:51 PM
 */
public interface WeightedVectorApproximation<T, E extends Exception> extends Approximation<WeightedVector<T>, E> {
    WeightedVector<T> currentResult();
}
