package net.fortytwo.laboratory.spotlight;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/*

cat tweets.sql | sed 's/.,[0-9]*,.t.,NULL,NULL..$//' | sed 's/.*','//' > tweets.txt
# delete the first two lines
vim tweets.txt

 */

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SpotlightTweetAnnotator {
    private static final String TWEET_FILE = "/tmp/tweets.txt";
    private static final boolean VERBOSE = true;

    public static void main(final String[] args) throws Exception {
        DefaultHttpClient httpclient = new DefaultHttpClient();

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();

        File tweetFile = new File(TWEET_FILE);
        BufferedReader br = new BufferedReader(new FileReader(tweetFile));
        String line;
        int count = 0;
        while ((line = br.readLine()) != null) {
            line = line.trim();
            if (line.length() > 0) {
                count++;
                //if (count > 10) {
                //    break;
                //}

                /*
curl http://localhost:2222/rest/annotate \
  -H "Accept: text/xml" \
  --data-urlencode "text=Brazilian state-run giant oil company Petrobras signed a three-year technology and research cooperation agreement with oil service provider Halliburton." \
  --data "confidence=0" \
  --data "support=0"
                 */

                long before = System.currentTimeMillis();

                HttpPost post = new HttpPost("http://localhost:2222/rest/annotate");
                //HttpPost post = new HttpPost("http://gemini.tw.rpi.edu:2222/rest/annotate");
                //HttpPost post = new HttpPost("http://spotlight.dbpedia.org/rest/annotate");

                post.setHeader("Accept", "text/xml");

                List<NameValuePair> formparams = new ArrayList<NameValuePair>();
                formparams.add(new BasicNameValuePair("text", line));
                formparams.add(new BasicNameValuePair("confidence", "0.2"));
                formparams.add(new BasicNameValuePair("support", "20"));
                UrlEncodedFormEntity requestEntity = new UrlEncodedFormEntity(formparams, "UTF-8");
                post.setEntity(requestEntity);

                HttpResponse response = httpclient.execute(post);
                long after = System.currentTimeMillis();

                try {
                    System.out.println(line);
                    if (2 != response.getStatusLine().getStatusCode() / 100) {
                        System.out.println("\tERROR\t" + response.getStatusLine());
                        continue;
                    }

                    HttpEntity entity = response.getEntity();

                    Document doc = db.parse(entity.getContent());
                    Element root = doc.getDocumentElement();
                    NodeList l = root.getChildNodes();
                    if (l.getLength() > 0) {
                        Element resources = (Element) l.item(1);

                        NodeList resourceList = resources.getElementsByTagName("Resource");
                        for (int i = 0; i < resourceList.getLength(); i++) {
                            Element r = (Element) resourceList.item(i);
                            String uri = r.getAttribute("URI");
                            int support = Integer.valueOf(r.getAttribute("support"));
                            double similarityScore = Double.valueOf(r.getAttribute("similarityScore"));
                            double percentageOfSecondRank = Double.valueOf(r.getAttribute("percentageOfSecondRank"));

                            System.out.println("\t" + uri);
                        }
                    }

                    EntityUtils.consume(entity);
                } finally {
                    post.releaseConnection();
                }

                long allDone = System.currentTimeMillis();
                if (VERBOSE) {
                    System.out.println("\tfinished in " + (after - before) + "ms (" + (allDone - before) + "ms incuding output)");
                }
            }
        }
        br.close();
    }
}
