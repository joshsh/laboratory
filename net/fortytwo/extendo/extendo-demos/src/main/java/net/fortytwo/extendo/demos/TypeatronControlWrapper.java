package net.fortytwo.extendo.demos;

import com.sun.speech.freetts.Voice;
import com.sun.speech.freetts.VoiceManager;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscReceiver;
import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.ripple.StringUtils;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControlWrapper {

    protected static final Logger logger = Logger.getLogger(TypeatronControlWrapper.class.getName());

    private Voice voice;

    protected final TypeatronControl typeatron;
    private final Runtime runtime;

    protected TypeatronControlWrapper() throws OscControl.DeviceInitializationException {
        final OscReceiver receiver = new OscReceiver();

        // TODO: don't hard-code the agent URI
        ExtendoAgent agent = new ExtendoAgent("http://fortytwo.net/foaf#josh", true);
        createVoice();

        runtime = Runtime.getRuntime();

        SideEffects environment = new SideEffects() {
            @Override
            public void speak(String message) {
                System.out.println("SPEAK: " + message);

                //speakWithFreeTts(message);
                speakWithSystemCall(message);
            }

            @Override
            public void setStatus(String message) {
                System.out.println("STATUS: " + message);
            }
        };

        typeatron = new TypeatronControl(receiver, agent, environment);
    }

    private void speakWithFreeTts(final String message) {
        voice.speak(message);
    }

    private void speakWithSystemCall(final String message) {
        Process p = null;
        try {
            p = runtime.exec("say \"" + StringUtils.escapeString(message) + "\"");
        } catch (IOException e) {
            typeatron.sendWarningMessage();
            logger.log(Level.WARNING, "'say' command failed", e);
        }
        if (null != p) {
            int exitCode = 0;
            try {
                exitCode = p.waitFor();
            } catch (InterruptedException e) {
                typeatron.sendErrorMessage();
                logger.log(Level.SEVERE, "interrupted while waiting for 'say' command", e);
            }
            if (0 != exitCode) {
                typeatron.sendWarningMessage();
                logger.warning("'say' command failed with code " + exitCode);
            }
        }
    }

    private void createVoice() {
        String voiceName = "kevin";

        VoiceManager voiceManager = VoiceManager.getInstance();
        voice = voiceManager.getVoice(voiceName);

        if (null == voice) {
            throw new IllegalStateException("Cannot find a voice named " + voiceName);
        }

        voice.allocate();
    }

    private void destroyVoice() {
        if (null != voice) {
            voice.deallocate();
        }
    }
}
