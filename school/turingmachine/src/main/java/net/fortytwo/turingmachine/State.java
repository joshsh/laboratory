package net.fortytwo.turingmachine;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Aug 29, 2008
 * Time: 4:54:13 PM
 * To change this template use File | Settings | File Templates.
 */
public interface State {
    boolean isAcceptState();
    boolean isRejectState();
}
