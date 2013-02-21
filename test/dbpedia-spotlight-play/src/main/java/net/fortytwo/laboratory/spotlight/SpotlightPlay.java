package net.fortytwo.laboratory.spotlight;

import org.dbpedia.spotlight.annotate.DefaultParagraphAnnotator;
import org.dbpedia.spotlight.disambiguate.ParagraphDisambiguatorJ;
import org.dbpedia.spotlight.disambiguate.TwoStepDisambiguator;
import org.dbpedia.spotlight.model.SpotlightConfiguration;
import org.dbpedia.spotlight.model.SpotlightFactory;
import org.dbpedia.spotlight.model.Text;
import org.dbpedia.spotlight.spot.Spotter;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SpotlightPlay {
    public static void main(final String[] args) throws Exception {
        String textStr = "Observations with NASA's Chandra X-ray Observatory have provided the first X-ray evidence of a supernova shock wave breaking through a cocoon of gas";
        Text text = new Text(textStr);
        SpotlightConfiguration conf = new SpotlightConfiguration("/tmp/spotlight.props");
        SpotlightFactory factory = new SpotlightFactory(conf);
        ParagraphDisambiguatorJ disambiguator = new ParagraphDisambiguatorJ(
                new TwoStepDisambiguator(factory.candidateSearcher(), factory.contextSearcher()));
        Spotter spotter = factory.spotter();
        DefaultParagraphAnnotator annotator = new DefaultParagraphAnnotator(spotter, disambiguator);
        System.out.println(annotator.annotate(textStr));

    }
}
