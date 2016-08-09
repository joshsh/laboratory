package net.fortytwo.conlang;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class PhonemeCollection {
    private static final Random random = new Random();

    private final List<String> accepted = new ArrayList<String>();
    private final List<String> potential = new ArrayList<String>();

    public PhonemeCollection(String... raw) {
        for (String s : raw) {
            if (!s.startsWith("(")) {
                s = s
                        .replaceAll("ng", "\u00f1")
                        .replaceAll("aa", "\u00e4")
                        .replaceAll("ee", "\u00eb")
                        .replaceAll("ii", "\u00ef")
                        .replaceAll("oe", "\u00f6")
                        .replaceAll("ue", "\u00fc");
                if (s.endsWith("?")) potential.add(s.substring(0, s.length() - 1));
                else accepted.add(s);
            }
        }
    }

    public String getRandom() {
        List<String> list = potential.size() > 0 && random.nextDouble() < 0.25 ? potential : accepted;

        int i = (int) (random.nextDouble() * list.size());
        return list.get(i);
    }
}
