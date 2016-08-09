import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;

public class TWedLightningTalksPage {
    private static String template = "<tr xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\" xmlns:tw=\"http://tw.rpi.edu/schema/\" xmlns:owl=\"http://www.w3.org/2002/07/owl#\" xmlns:foaf=\"http://xmlns.com/foaf/0.1/\" class=\"person-list-item\" about=\"PERSON_URI\"><td class=\"center\"><img src=\"PERSON_IMAGE\" alt=\"PERSON_NAME\" style=\"width: 75px\"><a rel=\"foaf:depiction\" href=\"PERSON_URI\"></a></td><td><a rel=\"foaf:page\" href=\"PERSON_URI\"><span property=\"foaf:name\">PERSON_NAME</span></a><br/>Lightning talk 2012-04-25: <a href=\"VIDEO_URL\">VIDEO_NAME</a></td></tr>";

    public static void main(final String[] args) throws Exception {
        System.out.println("<table>");

        InputStream in = new FileInputStream(new File("/tmp/TWed_lightning_talks_2012-04-25.csv"));
        BufferedReader r = new BufferedReader(new InputStreamReader(in));
        String line;

        while ((line = r.readLine()) != null) {
            String[] a = line.split(",");
            String personUri = a[0];
            String imageUrl = a[1];
            String personName = a[2];
            String videoUrl = a[3];
            String talkTitle = a[4];

            String tr = template.replaceAll("PERSON_URI", personUri)
                    .replaceAll("PERSON_IMAGE", imageUrl)
                    .replaceAll("PERSON_NAME", personName)
                    .replaceAll("VIDEO_NAME", talkTitle)
                    .replaceAll("VIDEO_URL", videoUrl);

            System.out.println(tr);
        }
        in.close();

        System.out.println("</table>");
    }
}
