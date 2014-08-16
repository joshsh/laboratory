package net.fortytwo.extendo.demos.util;

import java.util.Arrays;

// Modified from
//     http://stackoverflow.com/questions/7988486/how-do-you-calculate-the-variance-median-and-standard-deviation-in-c-or-java
public class Statistics {
    private final double[] data;
    private final int size;
    private Double min, max;

    public Statistics(double[] data) {
        this.data = data;
        size = data.length;
    }

    public double getMin() {
        if (null == min && 0 < size) {
            min = Double.MAX_VALUE;
            for (double d : data) {
                if (d < min) {
                    min = d;
                }
            }
        }

        return min;
    }

    public double getMax() {
        if (null == max && 0 < size) {
            max = Double.MIN_VALUE;
            for (double d : data) {
                if (d > max) {
                    max = d;
                }
            }
        }

        return max;
    }

    public double getMean() {
        double sum = 0.0;
        for (double a : data)
            sum += a;
        return sum / size;
    }

    public double getVariance() {
        double mean = getMean();
        double temp = 0;
        for (double a : data)
            temp += (mean - a) * (mean - a);
        return temp / size;
    }

    public double getStdDev() {
        return Math.sqrt(getVariance());
    }

    public double median() {
        double[] b = new double[data.length];
        System.arraycopy(data, 0, b, 0, b.length);
        Arrays.sort(b);
        if (0 < size) {
            min = b[0];
            max = b[size - 1];
        }

        if (data.length % 2 == 0) {
            return (b[(b.length / 2) - 1] + b[b.length / 2]) / 2.0;
        } else {
            return b[b.length / 2];
        }
    }
}