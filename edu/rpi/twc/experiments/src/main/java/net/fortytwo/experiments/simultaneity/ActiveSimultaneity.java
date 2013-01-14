package net.fortytwo.experiments.simultaneity;

/**
 * User: josh
 * Date: Jun 2, 2010
 * Time: 2:47:45 PM
 */
public class ActiveSimultaneity {
    public static void main(final String[] args) throws Exception {
        testDelayOverhead();
    }

    /**
     * Program to determine whether a given multitasking computer is suitable
     * for performing simultaneity experiments.  Results indicate that my
     * MacBook Pro is suitable for producing precise delays:
     *
     * With 1000 iterations of 25ms delays, the overhead is less than 1%.
     * It should be kept in mind that this depends heavily on how the computer
     * is multitasking at a given time.
     *
     * 1000 x 2 -->   5.820305142211339%
     * 1000 x 25 -->  0.8959010544676127%
     * 1000 x 50 -->  0.31102958768641836%
     * 1000 x 100 --> 0.2334537182991799%
     * 100 x 250 -->  0.10389195236953568%
     *
     * @throws InterruptedException
     */
    private static void testDelayOverhead() throws InterruptedException {
        Object m = "";
        int iters = 1000;
        long delay = 50;

        long before = System.currentTimeMillis();

        for (int i = 0; i < iters; i++) {

            synchronized (m) {
                m.wait(delay);
            }
        }

        long after = System.currentTimeMillis();

        long tottime = after - before;
        System.out.println("total time: " + tottime + "ms");
        long ovhd = tottime - iters * delay;
        System.out.println("overhead: " + ovhd + "ms (" + (100 * (ovhd / ((double) tottime))) + "%)");
    }
}
