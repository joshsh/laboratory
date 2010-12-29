package net.fortytwo.droidspeak.jade;

import jade.core.behaviours.CyclicBehaviour;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 8:52:03 PM
 */
public class MyCyclicBehaviour extends CyclicBehaviour {
    private int count = 0;

    public void action() {
        count++;
        System.out.println("here we go again (" + count + ")...");
    }
}
