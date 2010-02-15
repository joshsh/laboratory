package net.fortytwo.homework.operatingsystems.scheduling;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Sep 18, 2008
 * Time: 2:21:26 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Scheduler {
    private final float contextSwitchingTime;

    public Scheduler(final float contextSwitchingTime ) {
        this.contextSwitchingTime = contextSwitchingTime;
    }

    public float getContextSwitchingTime() {
        return contextSwitchingTime;
    }
}
