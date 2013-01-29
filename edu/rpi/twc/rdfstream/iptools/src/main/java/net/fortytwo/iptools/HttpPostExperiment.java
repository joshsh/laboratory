package net.fortytwo.iptools;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;

/**
 * User: josh
 * Date: Sep 12, 2010
 * Time: 9:07:37 PM
 */
public class HttpPostExperiment {
    public static void main(final String[] args) {
        try {
            System.out.println("iterations\tsize\tms\tmessages_per_second\tbytes_per_second");
            for (int i = 100; i < 1000; i += 100) {
                doTest(i, 100);
            }
            for (int i = 1000; i < 10000; i += 250) {
                doTest(i, 100);
            }
            for (int i = 10000; i < 100000; i += 2500) {
                doTest(i, 100);
            }
            for (int i = 100000; i < 1000000; i += 25000) {
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

        URL url = new URL("http://flux.franz.com:8000/count");
//        URL url = new URL("http://fortytwo.net");
//        URL url = new URL("http://google.com");

        long before = System.currentTimeMillis();
        for (int i = 0; i < iterations; i++) {
            URLConnection conn = url.openConnection();
            conn.setDoOutput(true);
            OutputStreamWriter wr = new OutputStreamWriter(conn.getOutputStream());
            for (int j = 0; j < messageSize / 100; j++) {
                wr.write(data);
            }
            wr.flush();

            BufferedReader rd = new BufferedReader(new InputStreamReader(conn.getInputStream()));
            String line;
            while ((line = rd.readLine()) != null) {
                // Process line...
            }
            wr.close();
            rd.close();
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
