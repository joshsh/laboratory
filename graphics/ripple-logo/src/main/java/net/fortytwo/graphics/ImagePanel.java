package net.fortytwo.graphics;

import javax.swing.*;
import java.awt.*;

/**
 * User: josh
* Date: Sep 22, 2010
* Time: 11:50:15 PM
*/
class ImagePanel extends JPanel {
    private final Image image;

    public ImagePanel(final Image image) {
        this.image = image;
//setBackground( Color.WHITE );
    }

    //override paint method of panel
    public void paint(final Graphics g) {
//setBackground( Color.WHITE );
        g.drawImage(image, 0, 0, this);
    }
}
