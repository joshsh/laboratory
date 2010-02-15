package net.fortytwo.turingmachine.examples.busybeaver;

import net.fortytwo.turingmachine.State;

/**
 * Created by IntelliJ IDEA.
* User: josh
* Date: Aug 29, 2008
* Time: 6:27:46 PM
* To change this template use File | Settings | File Templates.
*/
class HaltingState implements State {
    public boolean isAcceptState() {
        return true;
    }

    public boolean isRejectState() {
        return false;
    }

    public String toString() {
        return "H";
    }
}
