package net.fortytwo.school.xixipaper;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Dec 12, 2008
 * Time: 9:16:07 PM
 * To change this template use File | Settings | File Templates.
 */
class ProbabilityDistribution {
    private double[] probabilities;

    public ProbabilityDistribution(final double[] probs) throws Exception {
        if (!isValidProbabilityDistribution(probs)) {
            throw new Exception("bad distribution: probabilities don't add to 1");
        }

        probabilities = probs;
    }

    public double findEntropy() {
        double sum = 0;

        for (double p : probabilities) {
            //System.out.println("p = " + p);
            //System.out.println("    Math.log(p) = " + Math.log(p));

            if (!closeToZero(p)) {
                sum -= p * Math.log(p);
            }
        }

        return sum;
    }

    private static boolean closeToZero(final double d) {
        // FIXME: this is a clumsy way of dealing with inexact floating point numbers.
        return Math.abs(d) < 0.00001;
    }

    private static boolean isValidProbabilityDistribution(final double[] dist) {
        double sum = 0;

        for (double d : dist) {
            sum += d;
        }

        return closeToZero(1.0 - sum);
    }
}
