package net.fortytwo.turingmachine.examples.busybeaver;

import net.fortytwo.turingmachine.State;

/**
 * Created by IntelliJ IDEA.
* User: josh
* Date: Aug 29, 2008
* Time: 6:27:29 PM
* To change this template use File | Settings | File Templates.
*/
class RunningState implements State {
    private final String name;
    private final int index;

    public RunningState(final int index,
                        final String name) {
        this.index = index;
        this.name = name;
    }

    public boolean isAcceptState() {
        return false;
    }

    public boolean isRejectState() {
        return false;
    }

    public int getIndex() {
        return index;
    }

    public String toString() {
        return name;
    }
}
