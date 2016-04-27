package net.fortytwo.extendo.demos.eval;

import java.util.Random;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RandomTransition {
    private static final Random random = new Random();

    public static void main(final String[] args) throws Exception {
        long cycleLength = 300;
        for (int j = 1; j <= 100; j++) {
            int count = 0;
            int cycles = 100000;
            int averageSecondsBetweenMoves = j;
            for (int i = 0; i < cycles; i++) {
                double probMove = 1.0 - Math.pow(0.5,
                        cycleLength * Math.sqrt(2) / (averageSecondsBetweenMoves * 1000.0));
                //System.out.println("probMove = " + probMove + ", r = " + random.nextDouble());
                if (random.nextDouble() < probMove) {
                    count++;
                }
            }

            double dwellTime = (cycles * cycleLength) / (count * 1000.0);
            //System.out.println("average dwell time: " + dwellTime + "s");
            System.out.println("" + j + "," + dwellTime);
        }
    }
}
