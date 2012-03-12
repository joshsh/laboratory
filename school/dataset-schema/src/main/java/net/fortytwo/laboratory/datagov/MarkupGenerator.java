package net.fortytwo.laboratory.datagov;

import org.apache.commons.io.IOUtils;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MarkupGenerator {
    private enum MarkupFormat {Microdata, RDFa}

    public static void main(final String[] args) {
        try {
            //generateDocument(MarkupFormat.Microdata);
            generateDocument(MarkupFormat.RDFa);
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void generateDocument(final MarkupFormat format) throws Exception {
        JSONObject o;

        File f = new File("data/dataset-metadata.json");
        InputStream is = new FileInputStream(f);
        try {
            o = new JSONObject(IOUtils.toString(is));
        } finally {
            is.close();
        }

        Map<String, Dataset> datasetsByUri = new HashMap<String, Dataset>();

        //System.out.println("o: " + o);
        JSONArray results = o.getJSONObject("results").getJSONArray("bindings");
        for (int i = 0; i < results.length(); i++) {
            JSONObject b = results.getJSONObject(i);

            String s = b.getJSONObject("s").getString("value");
            String title = b.getJSONObject("title").getString("value");
            String homepage = b.getJSONObject("homepage").getString("value");
            String country = b.getJSONObject("country").getString("value");
            String subject = b.getJSONObject("subject").getString("value");
            String desc = b.getJSONObject("desc").getString("value");
            JSONObject a = b.optJSONObject("agency");
            String agency = null;
            String agencyTitle = null;
            if (null != a) {
                agency = a.getString("value");
                agencyTitle = b.getJSONObject("aglabel").getString("value");
            }

            //System.out.println("s: " + s);

            Dataset d = datasetsByUri.get(s);
            if (null == d) {
                d = new Dataset();
                d.setUri(s);
                d.setTitle(title);
                d.setHomepage(homepage);
                d.setCountry(country);
                d.setDescription(desc);
                d.setAgencyTitle(agencyTitle);
                d.setSubjects(new HashSet<String>());
                datasetsByUri.put(s, d);
            }

            d.getSubjects().add(subject);
        }

        //System.out.println("" + datasetsByUri.values().size() + " datasets");

        StringBuilder sb = new StringBuilder();

        if (format == MarkupFormat.Microdata) {
            sb.append("<!DOCTYPE html>\n" +
                    "<html lang=\"en\">\n" +
                    "<head>\n" +
                    "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" +
                    "    <title>schema.org dataset extension example</title>\n" +
                    "</head>\n" +
                    "<body>\n");
            for (Dataset d : datasetsByUri.values()) {
                //System.out.println("uri: " + d.getUri());
                sb.append("<div itemscope=\"itemscope\" itemtype=\"http://schema.org/Dataset\">\n    <a href=\"")
                        .append(htmlEscape(d.getHomepage()))
                        .append("\"><span itemprop=\"name\">\n" + "        <b>")
                        .append(htmlEscape(d.getTitle()))
                        .append("</b>\n" + "    </span></a>\n" + "\n");
                sb.append("    <div><meta itemprop=\"url\" content=\"")
                        .append(htmlEscape(d.getHomepage()))
                        .append("\"/>\n" + "    <span itemprop=\"description\">")
                        .append(htmlEscape(d.getDescription()))
                        .append("</span></div>\n" + "\n");     // TODO
/*
            sb.append("    <div><meta itemprop=\"url\" content=\"")
                    .append(htmlEscape(d.getHomepage()))
                    .append("\"/>\n" + "    <meta itemprop=\"description\" content=\"")
                    .append(htmlEscape(d.getDescription()))
                    .append("\"></div>\n" + "\n");     // TODO
                    */
                if (null != d.getCountry()) {
                    String label = d.getCountry();
                    if (label.startsWith("http://dbpedia.org/resource/")) {
                        label = label.substring(label.lastIndexOf("/") + 1).replaceAll("_", " ");
                    }
                    sb.append("    <div><i>Country:</i>\n" + "    <a href=\"")
                            .append(htmlEscape(d.getCountry()))
                            .append("\"><span itemprop=\"spatialScope\" itemscope=\"itemscope\" itemtype=\"http://schema.org/Country\">\n" + "            <span itemprop=\"name\">")
                            .append(htmlEscape(label))
                            .append("</span>\n" + "        </span>\n" + "    </a></div>\n" + "\n");
                }
                if (null != d.getAgencyTitle()) {
                    sb.append("    <div><i>Publisher:</i>\n" + "    <span itemprop=\"publisher\" itemscope=\"itemscope\" itemtype=\"http://schema.org/Organization\">\n" + "            <span itemprop=\"name\">")
                            .append(htmlEscape(d.getAgencyTitle()))
                            .append("</span>\n" + "        </span>\n" + "    </div>\n" + "\n");
                }
                int size = d.getSubjects().size();
                if (size > 0) {
                    sb.append("    <i>Categories:</i>\n");
                    int count = 0;
                    for (String subject : d.getSubjects()) {
                        sb.append("    <span itemprop=\"category\">");
                        sb.append("<span itemscope=\"itemscope\" itemtype=\"http://schema.org/Text\">")
                                .append(htmlEscape(subject))
                                .append("</span></span>")
                                .append(count < size - 1 ? ",\n" : "\n");
                        count++;
                        //if (count >= 5) {
                        //    break;
                        //}
                    }
                }
                sb.append("</div>\n" +
                        "<br/>\n\n");
            }
            sb.append("</body></html>");
        } else if (format == MarkupFormat.RDFa) {
            sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
                    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\"\n" +
                    "        \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">\n" +
                    "<html xmlns=\"http://www.w3.org/1999/xhtml\"\n" +
                    "      xmlns:foaf=\"http://xmlns.com/foaf/0.1/\"\n" +
                    "      version=\"XHTML+RDFa 1.0\" xml:lang=\"en\">\n" +
                    "<head>\n" +
                    "    <title>dataset extension RDFa example</title>\n" +
                    "    <base href=\"http://tw.rpi.edu/dataset/\"/>\n" +
                    "    <meta property=\"dc:creator\" content=\"Joshua Shinavier\"/>\n" +
                    "    <link rel=\"foaf:primaryTopic\" href=\"http://fortytwo.net/foaf#josh\"/>\n" +
                    "</head>\n" +
                    "<body>\n");
            for (Dataset d : datasetsByUri.values()) {

                // TODO: datatype

                // TODO: don't conflate URI with homepage
                //System.out.println("uri: " + d.getUri());
                sb.append("<div about=\"" + htmlEscape(d.getHomepage()) + "\">\n"
                        + "<a href=\"")
                        .append(htmlEscape(d.getHomepage()))
                        .append("\"><span itemprop=\"name\">\n" + "        <b>")
                        .append(htmlEscape(d.getTitle()))
                        .append("</b>\n" + "    </span></a>\n" + "\n");
                sb.append("    <div><meta itemprop=\"url\" content=\"")
                        .append(htmlEscape(d.getHomepage()))
                        .append("\"/>\n" + "    <span itemprop=\"description\">")
                        .append(htmlEscape(d.getDescription()))
                        .append("</span></div>\n" + "\n");     // TODO

                if (null != d.getCountry()) {
                    String label = d.getCountry();
                    if (label.startsWith("http://dbpedia.org/resource/")) {
                        label = label.substring(label.lastIndexOf("/") + 1).replaceAll("_", " ");
                    }
                    sb.append("    <div><i>Country:</i>\n" + "    <a href=\"")
                            .append(htmlEscape(d.getCountry()))
                            .append("\"><span itemprop=\"spatialScope\" itemscope=\"itemscope\" itemtype=\"http://schema.org/Country\">\n" + "            <span itemprop=\"name\">")
                            .append(htmlEscape(label))
                            .append("</span>\n" + "        </span>\n" + "    </a></div>\n" + "\n");
                }
                if (null != d.getAgencyTitle()) {
                    sb.append("    <div><i>Publisher:</i>\n" + "    <span itemprop=\"publisher\" itemscope=\"itemscope\" itemtype=\"http://schema.org/Organization\">\n" + "            <span itemprop=\"name\">")
                            .append(htmlEscape(d.getAgencyTitle()))
                            .append("</span>\n" + "        </span>\n" + "    </div>\n" + "\n");
                }
                int size = d.getSubjects().size();
                if (size > 0) {
                    sb.append("    <i>Categories:</i>\n");
                    int count = 0;
                    for (String subject : d.getSubjects()) {
                        sb.append("    <span itemprop=\"category\">");
                        sb.append("<span itemscope=\"itemscope\" itemtype=\"http://schema.org/Text\">")
                                .append(htmlEscape(subject))
                                .append("</span></span>")
                                .append(count < size - 1 ? ",\n" : "\n");
                        count++;
                        //if (count >= 5) {
                        //    break;
                        //}
                    }
                }
                sb.append("</div>\n" +
                        "<br/>\n\n");


            }

            sb.append("</body></html>");
        }

        /*
        sb.append("    <a href=\"http://validator.w3.org/check/referer\">\n" +
                "        <img src=\"http://www.w3.org/Icons/valid-xhtml10\" alt=\"Validate XHTML\" height=\"31\" width=\"88\"/>\n" +
                "    </a>\n");
        */


        OutputStream out = new FileOutputStream(new File("data/results.html"));
        try {
            out.write(sb.toString().getBytes("UTF-8"));
        } finally {
            out.close();
        }
    }

    private static String htmlEscape(String s) {
        // TODO
        return s
                .replaceAll("&", "&amp;")
                .replaceAll("\"", "&quot;");
    }

    private static class Dataset {
        private String uri;
        private String title;
        private String homepage;
        private String country;
        private Set<String> subjects;
        private String description;
        private String agencyTitle;

        public String getUri() {
            return uri;
        }

        public void setUri(String uri) {
            this.uri = uri;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public String getHomepage() {
            return homepage;
        }

        public void setHomepage(String homepage) {
            this.homepage = homepage;
        }

        public String getCountry() {
            return country;
        }

        public void setCountry(String country) {
            this.country = country;
        }

        public Set<String> getSubjects() {
            return subjects;
        }

        public void setSubjects(Set<String> subjects) {
            this.subjects = subjects;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public String getAgencyTitle() {
            return agencyTitle;
        }

        public void setAgencyTitle(String agencyTitle) {
            this.agencyTitle = agencyTitle;
        }
    }
}
