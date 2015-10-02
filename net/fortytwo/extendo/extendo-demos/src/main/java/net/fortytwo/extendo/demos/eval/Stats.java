package net.fortytwo.extendo.demos.eval;

import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Stats {

    public static double findMean(List<Long> sample) {
        if (null == sample || sample.isEmpty()) {
            return Double.NaN;
        }

        double sum = 0;
        for (long l : sample) {
            sum += l;
        }
        return sum / sample.size();
    }

    public static double findStandardDeviation(List<Long> sample, double mean) {
        if (null == sample || sample.isEmpty()) {
            return Double.NaN;
        }

        double sum = 0;
        for (long l : sample) {
            double q = l - mean;
            sum += (q * q);
        }

        return Math.sqrt(sum/sample.size());
    }
}
