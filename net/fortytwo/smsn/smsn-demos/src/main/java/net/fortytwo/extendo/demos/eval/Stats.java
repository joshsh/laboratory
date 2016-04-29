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

        return Math.sqrt(sum / sample.size());
    }

    public static String toR(List<Long> sample, String name) {
        boolean first = true;
        StringBuilder sb = new StringBuilder(name).append(" <- c(");
        for (long l : sample) {
            if (first) first = false;
            else sb.append(", ");
            sb.append(l);
        }
        sb.append(")");
        return sb.toString();
    }
}
