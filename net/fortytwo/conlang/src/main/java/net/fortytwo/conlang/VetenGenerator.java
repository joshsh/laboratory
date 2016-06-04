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

        for (int j = 0; j < 20; j++) {
            for (int i = 0; i < 30; i++) {
                if (RANDOM.nextDouble() > 0.4) {
                    sb.append(randomNoun());
                } else {
                    sb.append(randomParticle());
                }

                sb.append(" ");
            }
            sb.append("\n");
        }

        System.out.println(sb.toString());
    }

    private static String randomSingleInitialConsonant() {
        return Phonemes.SINGLE_INITIAL_CONSONANTS.getRandom();
    }

    private static String randomPairedInitialConsonant() {
        return Phonemes.PAIRED_INITIAL_CONSONANTS.getRandom();
    }

    private static String randomInitialConsonant() {
        return RANDOM.nextDouble() > 0.5
                ? randomSingleInitialConsonant()
                : randomPairedInitialConsonant();
    }

    private static String randomSingleFinalConsonant() {
        return Phonemes.SINGLE_FINAL_CONSONANTS.getRandom();
    }

    private static String randomPairedFinalConsonant() {
        return Phonemes.PAIRED_FINAL_CONSONANTS.getRandom();
    }

    private static String randomShortVowel() {
        return Phonemes.SHORT_VOWELS.getRandom();
    }

    private static String randomLongVowel() {
        return RANDOM.nextDouble() > 0.5
                ? Phonemes.LONG_VOWELS.getRandom()
                : Phonemes.DIPHTHONGS.getRandom();
    }

    private static String randomNounStem() {
        StringBuilder sb = new StringBuilder();

        if (RANDOM.nextDouble() > 0.25) {
            sb.append(randomInitialConsonant());
        }

        if (RANDOM.nextDouble() > 0.5) {
            sb.append(randomShortVowel()).append(randomPairedFinalConsonant());
        } else {
            sb.append(randomLongVowel()).append(randomSingleFinalConsonant());
        }

        return sb.toString();
    }

    private static String randomNoun() {
        return randomNounStem() + (RANDOM.nextDouble() > 0.25
                ? Phonemes.NOUN_SUFFIXES.getRandom()
                : Phonemes.VERB_SUFFIXES.getRandom());
    }

    private static String randomParticle() {
        StringBuilder sb = new StringBuilder();

        if (RANDOM.nextDouble() > 0.25) {
            sb.append(randomInitialConsonant());
        }

        if (RANDOM.nextDouble() > 0.5) {
            sb.append(randomLongVowel());
        } else {
            sb.append(randomShortVowel());
            if (RANDOM.nextDouble() > 0.5) {
                sb.append(randomSingleFinalConsonant());
            }
        }

        return sb.toString();
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
