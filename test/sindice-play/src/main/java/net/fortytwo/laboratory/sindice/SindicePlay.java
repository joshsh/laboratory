package net.fortytwo.laboratory.sindice;

import com.sindice.Sindice;
import com.sindice.result.SearchResult;
import com.sindice.result.SearchResults;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SindicePlay {
    private void findOccurrences() throws Exception {
        Sindice sindice = new Sindice();

        /*
        SearchResults r = sindice.termSearch("Shinavier");
        int count = r.getTotalResults();
        System.out.println("" + count + " results");
        */

        Set<String> types = new HashSet<String>();
        InputStream in = SindicePlay.class.getResourceAsStream("schema-org-types.txt");
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(in));
            String line;
            while ((line = reader.readLine()) != null) {
                types.add(line.trim());
            }
        } finally {
            in.close();
        }

        System.out.println("querying Sindice for " + types.size() + " types");

        Map<String, Integer> domainOccurrences = new HashMap<String, Integer>();

        int cur = 1;
        for (String type : types) {
            SearchResults results = sindice.advancedSearch("rdf:type", "http://schema.org/" + type);
            int count = results.getTotalResults();
            System.out.println(type + "\t" + count);
            int pages = 0;
            while (null != results && 0 < results.size()) {
                for (SearchResult r : results) {
                    String link = r.getLink();
                    String domain = findDomain(link);
                    //System.out.println("\tdomain: " + domain);
                    if (domain.length() > 0) {
                        Integer c = domainOccurrences.get(domain);
                        if (null == c) {
                            c = 0;
                        }
                        c++;
                        domainOccurrences.put(domain, c);
                    }
                }

                if (++pages >= 100) {
                    break;
                }

                try {
                    results = results.nextPage();
                } catch (NullPointerException e) {   // Soft-fail for an apparent Sindice4j bug
                    results = null;
                }
            }

            OutputStream out = new FileOutputStream(new File("/tmp/ranking.txt"));
            PrintStream ps = new PrintStream(out);
            try {
                for (Map.Entry<String, Integer> e : domainOccurrences.entrySet()) {
                    ps.println(e.getKey() + "\t" + e.getValue());
                }
            } finally {
                out.close();
            }

            //if (++cur > 3) {
            //    break;
            //}
        }
    }

    private void createOccurrenceWordleText(final String fileName) throws Exception {
        Map<String, Integer> occurrences = new HashMap<String, Integer>();
        int sum = 0;

        InputStream in = SindicePlay.class.getResourceAsStream(fileName);
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(in));
            String line;
            while ((line = reader.readLine()) != null) {
                String[] a = line.split("\t");
                String type = a[0];
                Integer occ = Integer.valueOf(a[1]);
                occurrences.put(type, occ);
                sum += occ;
            }
        } finally {
            in.close();
        }

        int total = 10000;
        for (Map.Entry<String, Integer> e : occurrences.entrySet()) {
            String type = e.getKey();
            int occ = e.getValue();
            int times = (int) (total * (occ / (double) sum));

            for (int i = 0; i < times; i++) {
                System.out.print(type + " ");
            }
        }
    }

    private void createTypeOccurrenceWordleText() throws Exception {
        createOccurrenceWordleText("type-occurrence.txt");
    }

    private void createDomainOccurrenceWordleText() throws Exception {
        createOccurrenceWordleText("domain-occurrence.txt");
    }

    private static String findDomain(final String url) {
        int i = url.indexOf("://");
        if (i > 0) {
            String rest = url.substring(i + 3);
            int j = rest.indexOf("/");
            if (j > 0) {
                return rest.substring(0, j);
            } else {
                return rest;
            }
        } else {
            System.err.println("URL is of unknown URI scheme: " + url);
            return "";
        }
    }

    public static void main(final String[] args) {
        try {
            SindicePlay p = new SindicePlay();
            //p.findOccurrences();
            //p.createTypeOccurrenceWordleText();
            p.createDomainOccurrenceWordleText();
        } catch (Exception e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
