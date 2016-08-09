package net.fortytwo.laboratory.datagov;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStream;
import java.io.PrintStream;

public class DatasetMetadataDeduplicator {
    public static void main(final String[] args) throws Exception {
        OutputStream out = new FileOutputStream(new File("/tmp/watson-dedup.csv"));
        PrintStream ps = new PrintStream(out);

        long inc = 1073448 / 100;
        long count = 0;
        File file = new File("/tmp/watson.csv");
        BufferedReader br = new BufferedReader(new FileReader(file));
//BufferedReader br = new BufferedReader(new InputStreamReader(in));
        String line;
        String lastId = "";
        boolean dup = false;
        while ((line = br.readLine()) != null) {
            count++;
            if (0 == count % inc) {
                System.out.println("" + count + " lines");
            }
            //System.out.println("line: " + line);
            int i = line.indexOf(',');
            if (i < 0) {
                if (!dup) {
                    ps.println(line);
                }
            } else {
                String id = line.substring(i);
                if (id.equals(lastId)) {
                    dup = true;
                } else {
                    dup = false;
                    lastId = id;
                    ps.println(line);
                }
            }
        }
        br.close();

        out.close();
    }
}
