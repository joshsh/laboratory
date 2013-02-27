package net.fortytwo.laboratory.datagov;

import org.apache.commons.io.IOUtils;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MarkupGenerator {
    private static final String PREFIX = "http://fortytwo.net/tmp/datasets/markup/";
//    private static final String PREFIX = "file:///tmp/datasets/markup/";

    private static final String MARKUP_DIR = "/tmp/datasets/markup/";

    private enum MarkupFormat {Microdata, RDFa}

    private Map<String, Catalog> catalogsByUri = new HashMap<String, Catalog>();
    private Map<String, Dataset> datasetsByUri = new HashMap<String, Dataset>();

    private MarkupGenerator() {
        catalogsByUri = new HashMap<String, Catalog>();
        datasetsByUri = new HashMap<String, Dataset>();
    }

    private void generateMarkup(final MarkupFormat format) throws Exception {
        JSONObject metadata;

        File f = new File("/tmp/datasets/results/datasets.json");
        InputStream is = new FileInputStream(f);
        try {
            metadata = new JSONObject(IOUtils.toString(is));
        } finally {
            is.close();
        }

        catalogsByUri = new HashMap<String, Catalog>();
        datasetsByUri = new HashMap<String, Dataset>();

        JSONArray results = metadata.getJSONObject("results").getJSONArray("bindings");
        System.out.println("results.length() = " + results.length());
        for (int i = 0; i < results.length(); i++) {
            JSONObject b = results.getJSONObject(i);

            String s = b.getJSONObject("dataset").getString("value");
            String c = b.getJSONObject("catalog").getString("value");
            String cid = b.getJSONObject("catalog_id").getString("value");
            String title = b.getJSONObject("title").getString("value");
            String homepage = b.getJSONObject("homepage").getString("value");
            String country = b.getJSONObject("country").getString("value");
            String desc = b.getJSONObject("desc").getString("value");
            JSONObject a = b.optJSONObject("agency");
            String agencyTitle = null;
            if (null != a) {
                String agency = a.getString("value");
                agencyTitle = b.getJSONObject("agency_id").getString("value");
            }
            // Note: the contributor and contributor_id fields are currently not used

            //System.out.println("s: " + s);
            //System.out.println("\t c: " + c);

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

                Catalog cat = catalogsByUri.get(c);
                if (null == cat) {
                    cat = new Catalog();
                    cat.setUri(c);
                    cat.setTitle(cid);
                    catalogsByUri.put(c, cat);
                }

                cat.getDatasets().add(s);
            }
        }

        JSONObject subjects;
        f = new File("/tmp/datasets/results/subjects.json");
        is = new FileInputStream(f);
        try {
            subjects = new JSONObject(IOUtils.toString(is));
        } finally {
            is.close();
        }

        //System.out.println("o: " + o);
        results = subjects.getJSONObject("results").getJSONArray("bindings");
        System.out.println("# of subject associations = " + results.length());
        for (int i = 0; i < results.length(); i++) {
            JSONObject b = results.getJSONObject(i);

            String s = b.getJSONObject("dataset").getString("value");
            String subject = b.getJSONObject("subject").getString("value");
            Dataset d = datasetsByUri.get(s);
            if (null != d) {
                d.getSubjects().add(subject);
            }
        }

        //System.out.println("" + datasetsByUri.values().size() + " datasets");

        for (Catalog cat : catalogsByUri.values()) {
            generateCatalogPage(cat, format);
            for (String s : cat.getDatasets()) {
                Dataset d = datasetsByUri.get(s);
                generateDatasetPage(d, cat, format);
            }
        }

        StringBuilder sb = new StringBuilder();

        sb.append("<!DOCTYPE html>\n" +
                "<html lang=\"en\">\n" +
                "<head>\n" +
                "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" +
                "    <title>schema.org dataset extension example</title>\n" +
                "</head>\n" +
                "<body>\n");
        sb.append("<h1>All catalogs</h1>\n<ol>\n");

        for (Catalog cat : catalogsByUri.values()) {
            String fileName = "catalog-" + hashOf(cat.getUri()) + ".html";
            sb.append("<li><a href=\"")
                    .append(fileName)
                    .append("\">")
                    .append(cat.getTitle())
                    .append("</a></li>\n");
        }

        sb.append("</ol>\n");
        sb.append("</body></html>");

        OutputStream out = new FileOutputStream(new File(MARKUP_DIR + "index.html"));
        try {
            out.write(sb.toString().getBytes("UTF-8"));
        } finally {
            out.close();
        }
    }

    private void generateCatalogPageWithRDFa(final Catalog c,
                                             final PrintStream ps) throws IOException {

        ps.println("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" +
                "<head>\n" +
                "    <title>dataset extension RDFa example</title>\n" +
                "    <base href=\"http://tw.rpi.edu/dataset/\"/>\n" +
                "    <meta property=\"dc:creator\" content=\"Joshua Shinavier\"/>\n" +
                "</head>\n" +
                "<body>\n");

        String uri = catalogUri(c);

        ps.println("<div vocab=\"http://schema.org/\" prefix=\"dcat: http://www.w3.org/ns/dcat#\" typeof=\"DataCatalog dcat:Catalog\"");
        ps.println("     about=\"" + uri + "\">");
        ps.println("    <span property=\"name\"><b>" + htmlEscape(c.getTitle()) + "</b></span>");

        System.out.println("catalog URI: " + c.getUri());
        if (0 < c.getDatasets().size()) {
            ps.println("    <div><i>Datasets in this catalog:</i>");
            boolean first = true;
            for (String did : c.getDatasets()) {
                System.out.println("\tdataset URI: " + did); System.out.flush();
                Dataset d = datasetsByUri.get(did);
                String duri = datasetUri(d);

                if (first) {
                    first = false;
                } else {
                    ps.print(",");
                }
                ps.println("    <span rel=\"dataset\" resource=\"" + duri + "\">");
                ps.println("        <a href=\"" + duri + "\">");
                ps.println("            <span about=\"" + duri + "\" typeof=\"DataSet\">");
                ps.println("                <span property=\"name\">" + htmlEscape(d.getTitle()) + "</span>");
                ps.println("            </span>");
                ps.println("        </a>");
                ps.println("    </span>");
            }
            ps.println("    </div>");
            ps.println("");
        }
        ps.println("</div>");

        ps.println("</body></html>");
    }

    private void generateCatalogPageWithMicrodata(final Catalog c,
                                                  final OutputStream out) throws IOException {
        StringBuilder sb = new StringBuilder();

        sb.append("<!DOCTYPE html>\n" +
                "<html lang=\"en\">\n" +
                "<head>\n" +
                "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" +
                "    <title>schema.org dataset extension example</title>\n" +
                "</head>\n" +
                "<body>\n");
        sb.append("<h1>Datasets in " + htmlEscape(c.getTitle()) + "</h1>\n<br/>\n");

        for (String dUri : c.getDatasets()) {
            Dataset d = datasetsByUri.get(dUri);
            //System.out.println("uri: " + d.getUri());
            sb.append("<div itemscope=\"itemscope\"" +
                    " itemid=\"" + d.getUri() + "\"" +
                    " itemtype=\"http://schema.org/Dataset\">\n    <a href=\"")
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
                        .append("\"><span itemprop=\"spatial\" itemscope=\"itemscope\" itemtype=\"http://schema.org/Country\">\n" + "            <span itemprop=\"name\">")
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
                    sb.append("    <span itemprop=\"keyword\">");
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

        out.write(sb.toString().getBytes("UTF-8"));
    }

    private void generateDatasetPageWithRDFa(final Dataset d,
                                             final Catalog c,
                                             final PrintStream ps) throws IOException {

        ps.println("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" +
                "<head>\n" +
                "    <title>dataset extension RDFa example</title>\n" +
                "    <base href=\"http://tw.rpi.edu/dataset/\"/>\n" +
                "    <meta property=\"dc:creator\" content=\"Joshua Shinavier\"/>\n" +
                "</head>\n" +
                "<body>\n");

        String uri = datasetUri(d);
        String countryName = d.getCountry().substring(d.getCountry().lastIndexOf("/") + 1).replaceAll("_", " ");

        ps.println("<div vocab=\"http://schema.org/\" prefix=\"dcat: http://www.w3.org/ns/dcat#\" typeof=\"Dataset dcat:Dataset\"");
        ps.println("     about=\"" + uri + "\">");
        ps.println(" ");
        if (null != d.getHomepage()) {
            ps.println("    <meta property=\"url\" content=\"" + htmlEscape(d.getHomepage()) + "\"></meta>");
        }

        ps.println("");
        if (null != d.getHomepage()) {
            ps.println("    <span property=\"name\"><b><a href=\"" + htmlEscape(d.getHomepage()) + "\">" + htmlEscape(d.getTitle()) + "</a></b></span>");
        } else {
            ps.println("    <span property=\"name\"><b>" + htmlEscape(d.getTitle()) + "</b></span>");
        }
        //ps.println("");
        //ps.println("    (<span property=\"temporalCoverage\">2011</span>, version \"2011-Sep-13\")");
        ps.println("");
        ps.println("    <div property=\"description\">" + htmlEscape(d.getDescription()) + "</div>");
        ps.println("");
        ps.println("    <div rel=\"spatialCoverage\" resource=\"" + htmlEscape(d.getCountry()) + "\"><i>Country:</i>");
        ps.println("        <a href=\"" + htmlEscape(d.getCountry()) + "\">");
        ps.println("            <span about=\"" + htmlEscape(d.getCountry()) + "\" typeof=\"Country\">");
        ps.println("                <span property=\"name\">" + countryName + "</span>");
        ps.println("            </span>");
        ps.println("        </a>");
        ps.println("    </div>");
        ps.println("");
        if (null != d.getAgencyTitle()) {
            ps.println("    <div rel=\"publisher\"><i>Publisher:</i>");
            ps.println("        <span typeof=\"Organization\">");
            ps.println("            <span property=\"name\">" + htmlEscape(d.getAgencyTitle()) + "</span>");
            //ps.println("\t    (<span property=\"email\">dot at example dot org</span>)");
            ps.println("        </span>");
            ps.println("    </div>");
            ps.println("");
        }
        //ps.println("    <div><i>Topics:</i>");
        //ps.println("        <span rel=\"about\" resource=\"http://dbpedia.org/resource/Seismic_hazard\">");
        //ps.println("            <a href=\"http://en.wikipedia.org/wiki/Seismic_hazard\">");
        //ps.println("                <span property=\"name\">seismic hazard</span></a>");
        //ps.println("        </span>");
        //ps.println("    </div>");
        //ps.println("");
        if (0 < d.getSubjects().size()) {
            ps.println("    <div><i>Keywords:</i>");
            boolean first = true;
            for (String s : d.getSubjects()) {
                if (first) {
                    first = false;
                } else {
                    ps.print(",");
                }
                ps.print("        <span property=\"keyword\">" + htmlEscape(s) + "</span>");
            }
            ps.println("    </div>");
            ps.println("");
        }

        //ps.println("    <div rel=\"license\"><i>License:</i>");
        //ps.println("    \t<a href=\"http://opendatacommons.org/licenses/pddl/1.0/\">");
        //ps.println("\t<span typeof=\"Webpage\">");
        //ps.println("\t    <span property=\"name\">ODC Public Domain Dedication and Licence (PDDL)</span>");
        //ps.println("\t    <meta property=\"url\" content=\"http://opendatacommons.org/licenses/pddl/1.0/\"/>");
        //ps.println("\t</span>");
        //ps.println("\t</a>");
        //ps.println("    </div>");
        //ps.println("    ");
        //ps.println("    <div rel=\"distribution\"><i>Download:</i>");
        //ps.println("        <a href=\"http://example.org/downloads/seismic-hazard-zones.nt.gz\">");
        //ps.println("        <span typeof=\"DataDownload\">");
        //ps.println("            <meta property=\"encodingFormat\" content=\"text/plain\" />");
        //ps.println("            <meta property=\"contentUrl\" content=\"http://example.org/downloads/seismic-hazard-zones.nt.gz\" />");
        //ps.println("            <meta property=\"inLanguage\" content=\"en\" />");
        //ps.println("            <span property=\"description\">compressed N-Triples dump</span>,");
        //ps.println("            <meta property=\"datePublished\" content=\"2011-08-12\">August 12, 2011</meta>");
        //ps.println("            <meta property=\"contentSize\" content=\"13.9\">(13.97MB)</span>");
        //ps.println("\t</span>");
        //ps.println("\t</a>");
        //ps.println("    </div>");
        //ps.println("");

        String curi = catalogUri(c);
        ps.println("    <span rel=\"catalog\" resource=\"" + curi + "\">In catalog:");
        ps.println("        <a href=\"" + curi + "\">");
        ps.println("            <span about=\"" + curi + "\" typeof=\"DataCatalog\">");
        ps.println("                <span property=\"name\">" + htmlEscape(c.getTitle()) + "</span>");
        ps.println("            </span>");
        ps.println("        </a>");
        ps.println("    </span>");

        ps.println("</div>");

        ps.println("</body></html>");

        /*
        StringBuilder sb = new StringBuilder();

        .append("        <div><b><a href=\"")
                .append(htmlEscape(d.getHomepage()))
                .append("\"><span about=\"")
                .append(htmlEscape(d.getUri()))
                .append("\"><span property=\"dcterms:title\">\n")
                .append(htmlEscape(d.getTitle()))
                .append("\n        </span></span></a></b></div>\n");

        //System.out.println("uri: " + d.getUri());
        sb.append("    <div about=\"" + htmlEscape(d.getUri()) + "\" typeof=\"dcat:Dataset\">\n");

        sb.append("<div>");
        sb.append("<h1>");
        if (null != d.getHomepage()) {
            sb.append("<a href=\"" + d.getHomepage() + "\">").append(d.getTitle()).append("</a>");
        } else {
            sb.append(d.getTitle());
        }
        sb.append("</h1>\n<br/>\n");
        sb.append("</div>\n");


        // TODO: URL (homepage)

        sb.append("        <div property=\"dcterms:description\">\n")
                .append(htmlEscape(d.getDescription()))
                .append("\n        </div>\n");

        if (null != d.getCountry()) {
            String label = d.getCountry();
            if (label.startsWith("http://dbpedia.org/resource/")) {
                label = label.substring(label.lastIndexOf("/") + 1).replaceAll("_", " ");
            }
            sb.append("        <div rel=\"dcterms:spatial\" resource=\"")
                    .append(htmlEscape(d.getCountry()))
                    .append("\"><i>Country:</i>\n")
                    .append("            <a href=\"")
                    .append(htmlEscape(d.getCountry()))
                    .append("\">\n")
                    .append("                <span about=\"")
                    .append(htmlEscape(d.getCountry()))
                    .append("\" typeof=\"adms:Country\">\n")
                    .append("                    <span property=\"dcterms:title\">")
                    .append(htmlEscape(label))
                    .append("</span>\n")
                    .append("                </span>\n")
                    .append("            </a>\n")
                    .append("        </div>\n");
        }

        if (null != d.getAgencyTitle()) {
            sb.append("        <div rel=\"dcterms:publisher\"><i>Publisher:</i>\n")
                    .append("            <span typeof=\"foaf:Organization\">\n")
                    .append("                <span property=\"dcterms:title\">")
                    .append(htmlEscape(d.getAgencyTitle()))
                    .append("</span>\n")
                    .append("            </span>\n")
                    .append("        </div>\n");
        }

        int size = d.getSubjects().size();
        if (size > 0) {
            sb.append("        <i>Categories:</i>\n");
            int count = 0;
            for (String subject : d.getSubjects()) {
                sb.append("        <span property=\"dcat:keyword\">")
                        .append(htmlEscape(subject))
                        .append("</span>")
                        .append(count < size - 1 ? ",\n" : "\n");
                count++;
                //if (count >= 5) {
                //    break;
                //}
            }
        }
        sb.append("    </div>\n")
                .append("    <br/>\n\n");

        sb.append("</body></html>");

        out.write(sb.toString().getBytes("UTF-8"));
        */
    }


    private void generateDatasetPageWithMicrodata(final Dataset d,
                                                  final OutputStream out) {

    }

    private void generateCatalogPage(final Catalog c,
                                     final MarkupFormat format) throws IOException {
        String fileName = "catalog-" + hashOf(c.getUri()) + ".html";

        OutputStream out = new FileOutputStream(new File(MARKUP_DIR + fileName));
        PrintStream ps = new PrintStream(out);
        try {
            //out.write(sb.toString().getBytes("UTF-8"));
            if (format == MarkupFormat.Microdata) {
                generateCatalogPageWithMicrodata(c, out);
            } else if (format == MarkupFormat.RDFa) {
                generateCatalogPageWithRDFa(c, ps);
            }
        } finally {
            out.close();
        }
    }

    private void generateDatasetPage(final Dataset d,
                                     final Catalog c,
                                     final MarkupFormat format) throws IOException {
        String fileName = "dataset-" + hashOf(d.getUri()) + ".html";

        OutputStream out = new FileOutputStream(new File(MARKUP_DIR + fileName));
        PrintStream ps = new PrintStream(out);
        try {
            //out.write(sb.toString().getBytes("UTF-8"));
            if (format == MarkupFormat.Microdata) {
                generateDatasetPageWithMicrodata(d, out);
            } else if (format == MarkupFormat.RDFa) {
                generateDatasetPageWithRDFa(d, c,  ps);
            }
        } finally {
            out.close();
        }
    }

    private static String catalogUri(final Catalog c) {
        return PREFIX + "catalog-" + hashOf(c.getUri()) + ".html#subject";
    }

    private static String datasetUri(final Dataset d) {
        return PREFIX + "dataset-" + hashOf(d.getUri()) + ".html#subject";
    }

    private static String htmlEscape(String s) {
        // TODO
        return s
                .replaceAll("&", "&amp;")
                .replaceAll("\"", "&quot;");
    }

    private static final MessageDigest DIGEST;

    static {
        try {
            DIGEST = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static String hashOf(final String key) {
        DIGEST.update(key.getBytes());
        String hash = "";
        byte[] digest = DIGEST.digest();
        for (byte b : digest) {
            String hex = Integer.toHexString(b);
            if (hex.length() == 1)
                hex = "0" + hex;
            hex = hex.substring(hex.length() - 2);
            hash = hash + hex;
        }
        return hash;
    }

    private static class Catalog {
        private String uri;
        private String title;
        private final Set<String> datasets = new HashSet<String>();

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

        public Set<String> getDatasets() {
            return datasets;
        }
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

    public static void main(final String[] args) {
        try {
            new MarkupGenerator().generateMarkup(MarkupFormat.RDFa);
            //generateDocument(MarkupFormat.RDFa);
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
