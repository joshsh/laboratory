package net.fortytwo.school.semanticescience.vsto;

import org.jdom.Element;

/**
 * Created by IntelliJ IDEA.
* User: josh
* Date: Sep 13, 2008
* Time: 7:00:25 PM
* To change this template use File | Settings | File Templates.
*/
class PaddleStyle {
    public static final PaddleStyle
            RED = new PaddleStyle("red"),
            YELLOW = new PaddleStyle("ylw"),
            GREEN = new PaddleStyle("grn"),
            BLUE = new PaddleStyle("blu"),
            WHITE = new PaddleStyle("wht");

    private String label;

    public PaddleStyle(final String label) {
        this.label = label;
    }

    public Element styleElement() {
        Element style = new Element("Style", VSTOGeoTagger.NS_KML);
        style.setAttribute("id", getPlacemarkID());

        Element iconStyle = new Element("IconStyle", VSTOGeoTagger.NS_KML);
        style.addContent(iconStyle);

        Element icon = new Element("Icon", VSTOGeoTagger.NS_KML);
        iconStyle.addContent(icon);

        Element href = new Element("href", VSTOGeoTagger.NS_KML);
        href.addContent("http://maps.google.com/mapfiles/kml/paddle/" + label + "-blank.png");
        icon.addContent(href);

        return style;

        /*        <Style id="normalPlacemark">
      <IconStyle>
        <Icon>
          <href>http://maps.google.com/mapfiles/kml/paddle/wht-blank.png</href>
        </Icon>
      </IconStyle>
    </Style>*/
    }

    public Element styleMapElement() {
        Element styleMapEl = new Element("StyleMap", VSTOGeoTagger.NS_KML);
        styleMapEl.setAttribute("id", getStyleMapID());

        Element pairEl, keyEl, styleUrlEl;

        pairEl = new Element("Pair", VSTOGeoTagger.NS_KML);
        styleMapEl.addContent(pairEl);
        keyEl = new Element("key", VSTOGeoTagger.NS_KML);
        keyEl.addContent("normal");
        pairEl.addContent(keyEl);
        styleUrlEl = new Element("styleUrl", VSTOGeoTagger.NS_KML);
        styleUrlEl.addContent("#" + getPlacemarkID());
        pairEl.addContent(styleUrlEl);

        return styleMapEl;

        /*
                <StyleMap id="exampleStyleMap">
          <Pair>
            <key>normal</key>
            <styleUrl>#normalPlacemark</styleUrl>
          </Pair>
          <Pair>
            <key>highlight</key>
            <styleUrl>#highlightPlacemark</styleUrl>
          </Pair>
        </StyleMap>*/
    }

    public String getStyleMapID() {
        return "stylemap_" + label;
    }

    public String getPlacemarkID() {
        return "placemark_" + label;
    }
}
