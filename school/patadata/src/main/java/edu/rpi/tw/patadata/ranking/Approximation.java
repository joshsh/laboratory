package edu.rpi.tw.patadata.ranking;

/**
 * User: josh
 * Date: Apr 19, 2010
 * Time: 1:33:07 PM
 */
public interface Approximation<E extends Exception> {
    /**
     * 
     * @param cycles the number of cycles available
     * @return the number of cycles actually consumed
     * @throws E an implementation-specific exception
     */
    int compute(int cycles) throws E;
}
