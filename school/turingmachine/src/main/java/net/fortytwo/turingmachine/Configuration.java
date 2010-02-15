package net.fortytwo.turingmachine;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 5:22:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class Configuration {
    private final ArrayList<Symbol> tape;

    private State currentState;
    private int currentHeadIndex;

    public Configuration(final State state,
                         final List<Symbol> u,
                         final List<Symbol> v) {
        currentState = state;

        if (0 != v.size() && Symbol.BLANK == v.get(v.size() - 1)) {
            throw new IllegalArgumentException("v may not end with a BLANK symbol");
        }

        tape = new ArrayList<Symbol>();
        tape.addAll(u);
        tape.addAll(v);

        currentHeadIndex = u.size();
    }

    public State getState() {
        return currentState;
    }

    public List<Symbol> getU() {
        return tape.subList(0, currentHeadIndex);
    }

    public List<Symbol> getV() {
        return tape.subList(currentHeadIndex, tape.size());
    }

    public void advance(final TuringMachine machine) {
        TransitionFunction f = machine.getTransitionFunction();

        DeltaTriplet result = f.apply(currentState, getCurrentSymbol());
//System.out.println("    result = " + result);
        setCurrentSymbol(result.getSymbol());
        currentState = result.getState();
        move(result.getDirection());
    }

    private Symbol getCurrentSymbol() {
        return (currentHeadIndex < tape.size())
                ? tape.get(currentHeadIndex)
                : Symbol.BLANK;
    }

    private void setCurrentSymbol(final Symbol s) {
        if (tape.size() == currentHeadIndex) {
            if (Symbol.BLANK != s) {
                tape.add(s);
            }
        } else if (tape.size() - 1 == currentHeadIndex && Symbol.BLANK == s) {
            tape.remove(tape.size() - 1);
        } else {
            tape.set(currentHeadIndex, s);
        }
    }

    private void move(final Direction d) {
        switch (d) {
            case LEFT:
                if (currentHeadIndex == tape.size() - 1 && Symbol.BLANK == tape.get(currentHeadIndex)) {
                    tape.remove(currentHeadIndex);
                }
                currentHeadIndex = (0 == currentHeadIndex)
                        ? 0
                        : currentHeadIndex - 1;
                break;
            case RIGHT:
                if (currentHeadIndex == tape.size()) {
                    tape.add(Symbol.BLANK);
                }
                currentHeadIndex++;
                break;
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append(currentState).append("\t:");

        for (int i = 0; i < tape.size(); i++) {
            Symbol s = tape.get(i);
            if (i == currentHeadIndex) {
                sb.append("[").append(s).append("]");
            } else {
                sb.append(s);
            }
        }

        if (tape.size() == currentHeadIndex) {
            sb.append("[").append(Symbol.BLANK).append("]");
        }

        return sb.toString();
    }
}
