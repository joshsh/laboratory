package net.fortytwo.homework.operatingsystems.scheduling;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Sep 18, 2008
 * Time: 2:18:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class Process {
    public enum Type { COMPUTATIONAL, INTERACTIVE };

    private final Type type;
    private final float burstTime;
    private final float diskServiceTime;
    private final String name;

    public Process(final Type type,
                   final float burstTime,
                   final float diskServiceTime,
                   final String name) {
        this.type = type;
        this.burstTime = burstTime;
        this.diskServiceTime = diskServiceTime;
        this.name = name;
    }

    public Type getType() {
        return type;
    }
    
    public float getBurstTime() {
        return burstTime;
    }

    public float getDiskServiceTime() {
        return diskServiceTime;
    }

    public String getName() {
        return name;
    }
}
