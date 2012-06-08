package net.fortytwo.extendo.hand;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import net.fortytwo.flow.NullSink;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class P5Gestures {
    private int PORT = 1331;

    private Map<String, Command> commands = new HashMap<String, Command>();

    private Sink<OSCMessage> nullSink = new NullSink<OSCMessage>();
    private Sink<OSCMessage> debugSink = new DebugSink();
    private RedirectableSink<OSCMessage> messageSink = new RedirectableSink<OSCMessage>(nullSink);

    private P5Vector lastSample;
    private Map<String, P5Vector> namedSamples = new HashMap<String, P5Vector>();

    public static void main(final String[] args) throws Exception {
        P5Gestures g = new P5Gestures();

        g.startListener();
        g.startIOLoop();
    }

    private P5Gestures() {
        messageSink.setDownstreamSink(nullSink);

        registerCommand(new ClassifyCommand());
        registerCommand(new DebugCommand());
        registerCommand(new DefineCommand());
        registerCommand(new PrintCommand());
        registerCommand(new SampleCommand());
        registerCommand(new UndefineCommand());
    }

    private void registerCommand(final Command command) {
        commands.put(command.getName(), command);
    }

    private void startListener() {
        Thread t = new Thread() {
            public void run() {
                try {
                    System.out.println("listening on port " + PORT);
                    OSCPortIn receiver = new OSCPortIn(PORT);
                    OSCListener listener = new OSCListener() {
                        public void acceptMessage(java.util.Date time, OSCMessage message) {
                            try {
                                messageSink.put(message);
                            } catch (Throwable e) {
                                System.err.println("error in OSC message handler");
                                e.printStackTrace(System.err);
                                System.exit(1);
                            }
                        }
                    };
                    receiver.addListener("/p5-out", listener);
                    receiver.startListening();
                } catch (Throwable t) {
                    System.err.println("error in OSC listener thread");
                    t.printStackTrace(System.err);
                    System.exit(1);
                }
            }
        };

        t.start();
    }

    private void startIOLoop() throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String line;

        List<String> commandNames = new LinkedList<String>();
        commandNames.addAll(commands.keySet());
        commandNames.add("exit");
        Collections.sort(commandNames);
        StringBuilder sb = new StringBuilder("commands: ");
        boolean first = true;
        for (String name : commandNames) {
            if (first) {
                first = false;
            } else {
                sb.append(", ");
            }

            sb.append(name);
        }

        System.out.println(sb);
        System.out.print("input> ");
        while (null != (line = br.readLine())) {
            line = line.replaceAll("\\s+", " ").trim();
            if (0 == line.length()) {
                continue;
            }

            String[] tokens = line.split(" ");

            String name = tokens[0];

            if (name.equals("exit")) {
                System.out.println("exiting");
                break;
            }

            Command c = commands.get(name);

            if (null == c) {
                System.out.println("unknown command: " + name);
            } else {
                try {
                    c.execute(tokens, br);
                } catch (CommandException e) {
                    System.out.println("error: " + e.getMessage());
                }
            }

            System.out.print("input> ");
        }
    }

    private static class CommandException extends Exception {
        public CommandException(final String message) {
            super(message);
        }
    }

    private abstract class Command {
        private String[] args;
        private BufferedReader reader;

        protected abstract void execute() throws CommandException;

        protected abstract int arity();

        public abstract String getName();

        public void execute(final String[] args,
                            final BufferedReader reader) throws CommandException {
            if (args.length < arity() + 1) {
                throw new CommandException("missing arguments");
            } else if (args.length > arity() + 1) {
                throw new CommandException("too many arguments");
            }

            this.args = args;
            this.reader = reader;
            execute();
        }

        protected String getArgument(final int index) throws CommandException {
            return args[index + 1];
        }

        protected void waitForEnter() {
            try {
                reader.readLine();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }


    private class SampleCommand extends Command {
        protected void execute() throws CommandException {
            SampleSink s = new SampleSink();
            messageSink.setDownstreamSink(s);

            System.out.println("Sampling... press ENTER to stop and compute average");
            waitForEnter();

            P5Vector avg = s.getAverage();

            messageSink.setDownstreamSink(nullSink);

            System.out.println("\tdone");

            lastSample = avg;
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "sample";
        }
    }

    private class DebugCommand extends Command {
        protected void execute() throws CommandException {
            messageSink.setDownstreamSink(debugSink);

            System.out.println("Displaying OSC messages... press ENTER to stop");
            waitForEnter();

            messageSink.setDownstreamSink(nullSink);

            System.out.println("\tdone");
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "debug";
        }
    }

    private class DefineCommand extends Command {
        protected void execute() throws CommandException {
            if (null == lastSample) {
                throw new CommandException("you have not yet taken a sample");
            } else {
                String name = getArgument(0);
                namedSamples.put(name, lastSample);
                System.out.println("defined '" + name + "' as last sample");
            }
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "define";
        }
    }

    private class PrintCommand extends Command {
        protected void execute() throws CommandException {
            String name = getArgument(0);

            P5Vector v = namedSamples.get(name);

            if (null == v) {
                throw new CommandException("there is no sample named '" + name + "'");
            } else {
                System.out.println("sample '" + name + "': " + v);
            }
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "print";
        }
    }

    private class UndefineCommand extends Command {
        protected void execute() throws CommandException {
            String name = getArgument(0);

            namedSamples.put(name, null);
            System.out.println("undefined '" + name + "'");
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "undefine";
        }
    }

    private class ClassifyCommand extends Command {
        protected void execute() throws CommandException {
            if (null == lastSample) {
                throw new CommandException("you have not yet taken a sample");
            } else if (0 == namedSamples.size()) {
                throw new CommandException("no vectors have been defined yet");
            } else {
                String best = null;
                double min = -1;

                for (Map.Entry<String, P5Vector> e : namedSamples.entrySet()) {
                    double dist = lastSample.distanceFrom(e.getValue());

                    if (min < 0 || dist < min) {
                        best = e.getKey();
                        min = dist;
                    }
                }

                System.out.println("closest match is '" + best + "'");
            }
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "classify";
        }
    }

    private static class P5Vector implements Cloneable {
        public final double[] components;

        public P5Vector(final int dimensions) {
            components = new double[dimensions];
            for (int i = 0; i < components.length; i++) {
                components[i] = 0;
            }
        }

        public P5Vector(double... components) {
            this.components = components;
        }

        public P5Vector(final P5Vector other) {
            this(other.components.clone());
        }

        /*public void add(final P5Vector other) {
            for (int i = 0; i < components.length; i++) {
                components[i] += other.components[i];
            }
        }*/

        public void multiplyBy(final double scalar) {
            for (int i = 0; i < components.length; i++) {
                components[i] *= scalar;
            }
        }

        public double distanceFrom(final P5Vector other) {
            double sum = 0;
            for (int i = 0; i < components.length; i++) {
                double diff = components[i] - other.components[i];
                sum += diff * diff;
            }

            return Math.sqrt(sum);
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < components.length; i++) {
                if (0 != i) {
                    sb.append(" ");
                }

                sb.append(components[i]);
            }

            return sb.toString();
        }
    }

    private class DebugSink implements Sink<OSCMessage> {
        public void put(OSCMessage message) throws RippleException {
            StringBuilder sb = new StringBuilder();
            for (Object a : message.getArguments()) {
                sb.append("\t").append(a);
            }
            System.out.println(sb);
        }
    }

    private class SampleSink implements Sink<OSCMessage> {
        private final P5Vector sum = new P5Vector(5);
        private int count = 0;

        public void put(OSCMessage message) throws RippleException {
            int i = 0;
            for (Object a : message.getArguments()) {
                sum.components[i] += new Integer(a.toString());
                i++;
            }
            count++;
        }

        public P5Vector getAverage() {
            P5Vector avg = new P5Vector(sum);
            if (count > 0) {
                avg.multiplyBy(1f / count);
            }
            return avg;
        }
    }

    private class RedirectableSink<T> implements Sink<T> {
        private Sink<T> downstreamSink;

        public RedirectableSink(final Sink<T> downstreamSink) {
            this.downstreamSink = downstreamSink;
        }

        public void setDownstreamSink(final Sink<T> downstreamSink) {
            this.downstreamSink = downstreamSink;
        }

        public void put(T t) throws RippleException {
            downstreamSink.put(t);
        }
    }
}
