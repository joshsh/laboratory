package net.fortytwo.droidspeak.jade;

import jade.core.AID;
import jade.core.Agent;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 7:31:11 PM
 */
public class JadePlay {
    public static void main(final String[] args) throws Exception {


        String nickname = "Josh's agent";
        AID id = new AID(nickname, AID.ISLOCALNAME);

        Agent a = new BookBuyerAgent();
        a.run();
    }
}
