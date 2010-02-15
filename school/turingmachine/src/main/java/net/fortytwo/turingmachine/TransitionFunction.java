package net.fortytwo.turingmachine;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 5:01:04 PM
 * To change this template use File | Settings | File Templates.
 */
public interface TransitionFunction {
    DeltaTriplet apply(State state, Symbol symbol);
}
