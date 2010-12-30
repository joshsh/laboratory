package net.fortytwo.droidspeak.jade;

import jade.core.behaviours.Behaviour;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 8:45:54 PM
 */
public class StrangeBehaviour extends Behaviour {
    private int c = 10;

    public void action() {
        System.out.println("" + c);
        c--;
    }

    public boolean done() {
        return 0 == c;
    }
}
