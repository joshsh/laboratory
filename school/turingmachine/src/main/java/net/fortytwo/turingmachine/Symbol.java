package net.fortytwo.turingmachine;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 4:55:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class Symbol implements Comparable<Symbol> {
    public static final Symbol BLANK = new Symbol(' ');
    private static int blanks = 0;

    private final char representation;

    public Symbol(final char r) {
        if (' ' == r) {
            if (blanks > 0) {
                throw new IllegalArgumentException("this character is reserved for the BLANK symbol: '" + r + "'");
            }
            blanks++;
        }

        if ('[' == r || ']' == r || ':' == r) {
            throw new IllegalArgumentException("this characters is reserved for formatting: '" + r + "'");
        }

        this.representation = r;
    }

    public int compareTo(final Symbol other) {
        return this.representation < other.representation
                ? -1
                : this.representation > other.representation
                ? 1
                : 0;
    }

    public String toString() {
        return "" + representation;
    }
}
