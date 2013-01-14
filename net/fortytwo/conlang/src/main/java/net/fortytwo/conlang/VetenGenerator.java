package net.fortytwo.conlang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VetenGenerator {
    private static final List<String> vowels;
    private static final List<String> consonants;
    private static final List<String> suffixes;
    private static final Map<String, Double> medialConsonants;

    static {
        try {
            vowels = getLines("vowels.txt");
            consonants = getLines("consonants.txt");
            suffixes = getLines("suffixes.txt");
            medialConsonants = getMap("medial-consonants.txt");
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static final Random RANDOM = new Random();

    public static void main(final String[] args) {
        try {
            //generatePairs();
            generateWords();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void generateWords() throws Exception {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < 50; i++) {
            if (RANDOM.nextDouble() > 0.4) {
                sb.append(randomNoun());
            } else {
                sb.append(randomParticle());
            }

            sb.append(" ");
        }

        System.out.println(sb.toString());
    }

    private static String randomNoun() {
        StringBuilder sb = new StringBuilder();

        if (RANDOM.nextDouble() > 0.25) {
            sb.append(chooseRandom(consonants));
        }

        sb.append(chooseRandom(vowels));

        if (RANDOM.nextDouble() > 0.25) {
            sb.append(chooseRandom(medialConsonants, 3));
            sb.append(chooseRandom(suffixes));
        }

        return sb.toString();
    }

    private static String randomParticle() {
        StringBuilder sb = new StringBuilder();

        if (RANDOM.nextDouble() > 0.25) {
            sb.append(chooseRandom(consonants));
        }

        sb.append(chooseRandom(vowels));

        if (RANDOM.nextDouble() > 0.25) {
            sb.append(chooseRandom(consonants));
        }

        return sb.toString();
    }

    private static void generatePairs() throws Exception {
        for (String f : consonants) {
            System.out.println(f);
            for (String s : consonants) {
                if (!s.equals(f)) {
                    System.out.println(f + s);
                }
            }
        }
    }

    private static String chooseRandom(final List<String> options) {
        return options.get(RANDOM.nextInt(options.size()));
    }

    private static String chooseRandom(final Map<String, Double> options,
                                       final double min) {
        String[] keys = new String[options.size()];
        options.keySet().toArray(keys);
        while (true) {
            String o = keys[RANDOM.nextInt(keys.length)];
            if (options.get(o) >= min) {
                return o;
            }
        }
    }

    private static Map<String, Double> getMap(final String fileName) throws IOException {
        Map<String, Double> map = new HashMap<String, Double>();

        for (String line : getLines(fileName)) {
            int i = line.indexOf(" ");
            if (i >= 0) {
                int j = line.lastIndexOf(" ");
                map.put(line.substring(0, i), Double.valueOf(line.substring(j + 1)));
            }
        }

        return map;
    }

    private static List<String> getLines(final String fileName) throws IOException {
        List<String> lines = new LinkedList<String>();

        InputStream in = VetenGenerator.class.getResourceAsStream(fileName);
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));//, "UTF-8"));
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (0 < line.length()) {
                    lines.add(line);
                }
            }
        } finally {
            in.close();
        }

        return lines;
    }
}
