package net.fortytwo.extendo.demos;

import edu.uci.ics.jung.algorithms.cluster.EdgeBetweennessClusterer;
import edu.uci.ics.jung.algorithms.cluster.VoltageClusterer;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Pair;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PersonClusterer {
    public PersonClusterer(final File adjacencyCsv) throws IOException {
        Map<String, String> vertices = new HashMap<String, String>();
        int edgeCount = 0;

        long startTime = System.currentTimeMillis();
        Graph<String, String> g = new DirectedSparseGraph<String, String>();
        InputStream in = new FileInputStream(adjacencyCsv);
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = br.readLine())) {
                String[] parts = line.trim().split(",");
                if (2 != parts.length) throw new IllegalStateException();

                String from = parts[0], to = parts[1];
                if (!vertices.keySet().contains(from)) {
                    vertices.put(from, from);
                } else {
                    from = vertices.get(from);
                }

                if (!vertices.keySet().contains(to)) {
                    vertices.put(to, to);
                } else {
                    to = vertices.get(to);
                }

                g.addEdge("e" + (edgeCount++), from, to);
            }
        } finally {
            in.close();
        }
        long endTime = System.currentTimeMillis();
        System.out.println("loaded " + edgeCount + " edges in " + (endTime - startTime) + "ms");

        //*
        OutputStream out = new FileOutputStream(new File("/tmp/graph-undirected.tsv"));
        PrintStream ps = new PrintStream(out);
        for (String e : g.getEdges()) {
            Pair<String> p = g.getEndpoints(e);
            //System.out.println(p.getFirst() + "\t" + p.getSecond());
            ps.println(p.getFirst().compareTo(p.getSecond()) <= 0
                    ? p.getFirst() + "\t" + p.getSecond()
                    : p.getSecond() + "\t" + p.getFirst());
        }
        out.close();
        //*/

        startTime = System.currentTimeMillis();

        //VoltageClusterer vc = new VoltageClusterer(g, 3);
        //Collection<Set> clusters = vc.cluster(3);
        EdgeBetweennessClusterer ec = new EdgeBetweennessClusterer(edgeCount - 5);
        Set<Set> clusters = ec.transform(g);

        endTime = System.currentTimeMillis();
        System.out.println("found " + clusters.size() + " clusters in " + (endTime - startTime) + "ms");
    }

    public static void main(final String[] args) throws Exception {
        PersonClusterer pc = new PersonClusterer(new File("/tmp/foafKnows.csv"));
    }
}
