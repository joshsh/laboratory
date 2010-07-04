package net.fortytwo.sesamize;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * User: josh
 * Date: Jul 4, 2010
 * Time: 2:25:33 PM
 */
class Args {
    public final Set<String> flags;
    public final Map<String, String> pairs;
    public final List<String> nonOptions;

    public Args(final String[] args) {
        flags = new HashSet<String>();
        pairs = new HashMap<String, String>();
        nonOptions = new LinkedList<String>();

        boolean inOption = false;
        for (int i = 0; i < args.length; i++) {
            String a = args[i];
            if (a.startsWith("-")) {
                if (inOption) {
                    flags.add(getOptionName(args[i - 1]));
                } else {
                    inOption = true;
                }
            } else {
                if (inOption) {
                    pairs.put(getOptionName(args[i - 1]), a);
                    inOption = false;
                } else {
                    nonOptions.add(a);
                }
            }
        }
    }

    private String getOptionName(final String option) {
        if (option.startsWith("--")) {
            return option.substring(2);
        } else if (option.startsWith("-")) {
            return option.substring(1);
        } else {
            return option;
        }
    }

}
