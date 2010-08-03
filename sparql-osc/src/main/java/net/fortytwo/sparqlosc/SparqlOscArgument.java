package net.fortytwo.sparqlosc;

/**
 * User: josh
 * Date: Aug 3, 2010
 * Time: 3:10:04 PM
 */
public class SparqlOscArgument {
    private final String name;
    private final OscType type;

    public SparqlOscArgument(String name, OscType type) {
        this.name = name;
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public OscType getType() {
        return type;
    }
}
