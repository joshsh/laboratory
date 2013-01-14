package net.fortytwo.turingmachine;

import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 4:51:34 PM
 * To change this template use File | Settings | File Templates.
 */
public class TuringMachine {
    private final Set<State> states;
    private final Set<Symbol> inputAlphabet;
    private final Set<Symbol> tapeAlphabet;
    private final TransitionFunction transitionFunction;
    private final State startState;

    public TuringMachine(final Set<State> states,
                         final Set<Symbol> inputAlphabet,
                         final Set<Symbol> tapeAlphabet,
                         final TransitionFunction transitionFunction,
                         final State startState) {
        this.states = states;
        this.inputAlphabet = inputAlphabet;
        this.tapeAlphabet = tapeAlphabet;
        this.transitionFunction = transitionFunction;
        this.startState = startState;

        validate();
    }

    private void validate() {
        // Note: not checking for null arguments

        if (!states.contains(startState)) {
            throw new IllegalArgumentException("given states do not contain the given start state");
        }

        // Note: not checking for existence of accepting or rejecting states

        if (inputAlphabet.contains(Symbol.BLANK)) {
            throw new IllegalArgumentException("BLANK symbol is not allowed in input alphabet");
        }

        if (!tapeAlphabet.contains(Symbol.BLANK)) {
            throw new IllegalArgumentException("BLANK symbol must be present in tape alphabet");
        }

        if (!tapeAlphabet.containsAll(inputAlphabet)) {
            throw new IllegalArgumentException("input alphabet must be a subset of tape alphabet");
        }
    }

    public Set<State> getStates() {
        return states;
    }

    public Set<Symbol> getInputAlphabet() {
        return inputAlphabet;
    }

    public Set<Symbol> getTapeAlphabet() {
        return tapeAlphabet;
    }

    public TransitionFunction getTransitionFunction() {
        return transitionFunction;
    }

    public State getStartState() {
        return startState;
    }
}
