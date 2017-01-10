package net.fortytwo.beam;

import org.apache.beam.sdk.Pipeline;
import org.apache.beam.sdk.io.TextIO;
import org.apache.beam.sdk.options.PipelineOptionsFactory;
import org.apache.beam.sdk.transforms.DoFn;
import org.apache.beam.sdk.transforms.GroupByKey;
import org.apache.beam.sdk.transforms.ParDo;
import org.apache.beam.sdk.transforms.windowing.Sessions;
import org.apache.beam.sdk.transforms.windowing.Window;
import org.apache.beam.sdk.values.KV;
import org.apache.beam.sdk.values.PCollection;
import org.joda.time.Duration;
import org.joda.time.Instant;

import java.io.Serializable;
import java.util.List;

public class BeamPlay {
    /* R
df <- data.frame(entity="ArthurDent", time=cumsum(rpois(100, lambda = 4)))
write.csv(df, file("/tmp/example.csv"), row.names=FALSE, quote=FALSE)
     */
    private void csvPipeline(final Pipeline p) {
        PCollection<String> lines = p.apply(
                //TextIO.Read.from("file:///tmp/example.csv"));
                TextIO.Read.from("file:///Users/josh/projects/fortytwo/laboratory/test/beam-play/src/test/resources/net/fortytwo/beam/session-data.csv"));

        lines
                .apply(ParDo.of(new ParseEventDoFn()))
                .apply(Window.into(Sessions.withGapDuration(Duration.millis(10))))
                .apply(GroupByKey.create())
                .apply((ParDo.of(new HandleEventSessionDoFn())));
    }

    private Pipeline createPipeline(final String[] args) {
        return Pipeline.create(PipelineOptionsFactory.fromArgs(args).create());
    }

    private static class ParseEventDoFn extends DoFn<String, KV<String, Event>> {
        @ProcessElement
        public void processElement(ProcessContext c) {
            String[] fields = c.element().trim().split(",");
            String userName = fields[0].trim();
            long timestamp = Long.valueOf(fields[1].trim());

            Event event = new Event(timestamp, "user:" + userName);
            KV<String, Event> kv = KV.of(userName, event);

            c.outputWithTimestamp(kv, new Instant(timestamp));
        }
    }

    private static class Event implements Serializable, Comparable<Event> {
        private final Long timestamp;
        private final String metadata;

        private Event(Long timestamp, String metadata) {
            this.metadata = metadata;
            this.timestamp = timestamp;
        }

        @Override
        public int compareTo(Event other) {
            return timestamp.compareTo(other.timestamp);
        }

        @Override
        public String toString() {
            return "(" + timestamp + ",\"" + metadata + "\")";
        }
    }

    private static class HandleEventSessionDoFn extends DoFn<Object, Object> {
        @DoFn.ProcessElement
        public void processElement(ProcessContext c) {
            Object el = c.element();
            KV kv = (KV) el;
            System.out.println(kv.getKey());
            List<Event> events = (List<Event>) kv.getValue();
            for (Event e : events) {
                System.out.println("\t" + e);
            }
        }
    }

    public static void main(String[] args) {
        args = new String[]{"--runner=direct"};
        //new BeamPlay().doSomething(args);
        BeamPlay play = new BeamPlay();
        Pipeline p = play.createPipeline(args);
        play.csvPipeline(p);
        //play.wordLengthPipeline(p);
        p.run();
    }
}
