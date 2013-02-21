package net.fortytwo.droidspeak.jade;

import jade.core.Agent;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 8:47:25 PM
 */
public class MyFirstAgent extends Agent {
    public MyFirstAgent() {
        // I haven't seen any documentation which says you can't add behaviours at construction time...
        this.addBehaviour(new StrangeBehaviour());
        this.addBehaviour(new MyOneShotBehaviour());
        //this.addBehaviour(new MyCyclicBehaviour());
    }
}
