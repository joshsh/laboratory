package net.fortytwo.school.xixipaper;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Dec 12, 2008
 * Time: 8:49:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class XixiPaper {
    public static void main(final String[] args) throws Exception {
        /*
        ProbabilityDistribution dist = new ProbabilityDistribution(new double[]
                {1 / 3.0, 1 / 3.0, 0, 1 / 6.0, 1 / 6.0});

        System.out.println("entropy = " + dist.findEntropy());
        */

        Relation minimal = new Relation(5, 5)
                .addPair(0, 1)
                .addPair(1, 0)
                .addPair(2, 1)
                .addPair(3, 0)
                .addPair(3, 3)
                .addPair(4, 4);

        Relation withExtraX = new Relation(10, 5);
        withExtraX.addPairs(minimal);

        Relation withExtraY = new Relation(5, 10);
        withExtraY.addPairs(minimal);

        Relation withExtraXandY = new Relation(10, 10);
        withExtraXandY.addPairs(minimal);

        System.out.println("simple entropy:");
        System.out.println("    minimal: " + simpleEntropy(minimal));
        System.out.println("    extra X: " + simpleEntropy(withExtraX));
        System.out.println("    extra Y: " + simpleEntropy(withExtraY));
        System.out.println("    extra X and Y: " + simpleEntropy(withExtraXandY));

        System.out.println("adjusted entropy:");
        System.out.println("    minimal: " + adjustedEntropy(minimal));
        System.out.println("    extra X: " + adjustedEntropy(withExtraX));
        System.out.println("    extra Y: " + adjustedEntropy(withExtraY));
        System.out.println("    extra X and Y: " + adjustedEntropy(withExtraXandY));
    }

    private static double simpleEntropy(final Relation r) throws Exception {
        return r.distributionInTermsOfRange().findEntropy();
    }

    private static double adjustedEntropy(final Relation r) throws Exception {
        return r.adjustedRelation().distributionInTermsOfRange().findEntropy();
    }
}
