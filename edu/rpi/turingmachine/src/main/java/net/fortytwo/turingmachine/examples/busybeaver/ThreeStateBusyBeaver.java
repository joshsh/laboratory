package net.fortytwo.turingmachine.examples.busybeaver;

import net.fortytwo.turingmachine.Configuration;
import net.fortytwo.turingmachine.DeltaTriplet;
import net.fortytwo.turingmachine.Direction;
import net.fortytwo.turingmachine.State;
import net.fortytwo.turingmachine.Symbol;
import net.fortytwo.turingmachine.TransitionFunction;
import net.fortytwo.turingmachine.TuringMachine;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 6:17:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class ThreeStateBusyBeaver extends TuringMachine {
    private static final RunningState
            stateA = new RunningState(0, "A"),
            stateB = new RunningState(1, "B"),
            stateC = new RunningState(2, "C");
    private static final HaltingState
            haltingState = new HaltingState();
    private static final Symbol ONE = new Symbol('1');

    public ThreeStateBusyBeaver() {
        super(createStates(), createInputAlphabet(), createTapeAlphabet(), createTransitionFunction(), stateA);
    }

    private static Set<Symbol> createTapeAlphabet() {
        Set<Symbol> s = new HashSet<Symbol>();
        s.add(Symbol.BLANK);
        s.add(ONE);
        return s;
    }

    private static Set<Symbol> createInputAlphabet() {
        Set<Symbol> s = new HashSet<Symbol>();
        s.add(ONE);
        return s;
    }

    private static Set<State> createStates() {
        Set<State> s = new HashSet<State>();
        s.add(stateA);
        s.add(stateB);
        s.add(stateC);
        s.add(haltingState);
        return s;
    }

    private static TransitionFunction createTransitionFunction() {
        final DeltaTriplet[][] table = {
                {
                        new DeltaTriplet(stateB, ONE, Direction.RIGHT),
                        new DeltaTriplet(stateC, Symbol.BLANK, Direction.RIGHT),
                        new DeltaTriplet(stateC, ONE, Direction.LEFT)
                },
                {
                        new DeltaTriplet(haltingState, ONE, Direction.RIGHT),
                        new DeltaTriplet(stateB, ONE, Direction.RIGHT),
                        new DeltaTriplet(stateA, ONE, Direction.LEFT)
                }
        };

        TransitionFunction f = new TransitionFunction() {

            public DeltaTriplet apply(final State state,
                                      final Symbol symbol) {
                int stateIndex = ((RunningState) state).getIndex();
                int symbolIndex = (Symbol.BLANK == symbol) ? 0 : 1;

                return table[symbolIndex][stateIndex];
            }
        };

        return f;
    }

    public static void main(final String[] args) throws Exception {
        TuringMachine t = new ThreeStateBusyBeaver();

        State startState = stateA;
        List<Symbol> u = new LinkedList<Symbol>();
        List<Symbol> v = new LinkedList<Symbol>();
        Configuration c = new Configuration(startState, u, v);

        for (int i = 1; i <= 100; i++) {
            System.out.println("" + i + ")\t" + c);

            State s = c.getState();
            if (s.isAcceptState() || s.isRejectState()) {
                break;
            }
            
            c.advance(t);
        }
    }
}
