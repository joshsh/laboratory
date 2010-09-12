package net.fortytwo.iptools;

import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

/**
 * User: josh
 * Date: Sep 13, 2010
 * Time: 12:46:51 AM
 */
public class HttpGetExperiment {
    public static void main(final String[] args) {
        try {
            System.out.println("iterations\tsize\tms\tmessages_per_second\tbytes_per_second");
            for (int i = 100; i < 1000; i += 100) {
                doTest(i, 100);
            }
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void doTest(final int messageSize,
                               final int iterations) throws Exception {
        String data = UdpExperiments.randomString(100);

        URL url = new URL("http://flux.franz.com:8000/bigget?size=" + messageSize);
//        URL url = new URL("http://fortytwo.net/Home.html");

        long before = System.currentTimeMillis();
        for (int i = 0; i < iterations; i++) {
            URLConnection conn = url.openConnection();
            conn.setDoInput(true);
            InputStreamReader wr = new InputStreamReader(conn.getInputStream());
            for (int j = 0; j < messageSize; j++) {
                wr.read();
            }
            wr.close();
        }
        long after = System.currentTimeMillis();
        long d = after - before;

        System.out.println("" + iterations
                + "\t" + messageSize
                + "\t" + d
                + "\t" + (1000 * iterations / (d * 1.0))
                + "\t" + (messageSize * 1000 * iterations / (d * 1.0)));
    }
}
