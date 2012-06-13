package net.fortytwo.turingmachine;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 5:03:16 PM
 * To change this template use File | Settings | File Templates.
 */
public class DeltaTriplet {
    private final State state;
    private final Symbol symbol;
    private final Direction direction;

    public DeltaTriplet(final State state,
                        final Symbol symbol,
                        final Direction direction) {
        this.state = state;
        this.symbol = symbol;
        this.direction = direction;
    }

    public State getState() {
        return state;
    }

    public Symbol getSymbol() {
        return symbol;
    }

    public Direction getDirection() {
        return direction;
    }

    public String toString() {
        return "(" + state + ", " + symbol + ", " + direction + ")";
    }
}
