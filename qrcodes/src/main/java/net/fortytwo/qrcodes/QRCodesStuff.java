package net.fortytwo.qrcodes;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.WriterException;
import com.google.zxing.common.ByteMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.UUID;

/**
 * User: josh
 * Date: Aug 13, 2010
 * Time: 5:53:38 PM
 */
public class QRCodesStuff {
    private static final QRCodeWriter w = new QRCodeWriter();

    public static void main(final String[] args) throws Exception {
        int rows = 1;// 11;
        int cols = 1;// 8;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                UUID u = UUID.randomUUID();
                String url = "http://myotherbrain.fortytwo.net/things/" + u;
                System.out.println(url);
                Image img = urlToQRCode(url, 200, 200);
                showImage(img);
            }
        }

        //urlToQRCode("http://example.org", 200, 200);
    }

    private static BufferedImage urlToQRCode(final String url,
                                             final int width,
                                             final int height) throws WriterException {
        ByteMatrix m = w.encode(url, BarcodeFormat.QR_CODE, width, height);
        return toImage(m);
    }

    // See: http://stackoverflow.com/questions/2489048/qr-code-encoding-and-decoding-using-zxing
    private static BufferedImage toImage(final ByteMatrix matrix) {
        int width = matrix.getWidth();
        int height = matrix.getHeight();

        byte[][] array = matrix.getArray();

        //create buffered image to draw to
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        //iterate through the matrix and draw the pixels to the image
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int grayValue = array[y][x] & 0xff;
                image.setRGB(x, y, (grayValue == 0 ? 0 : 0xFFFFFF));
            }
        }

        return image;
    }

    private static void showImage(final Image image) {
        JPanel panel = new JPanel() {
            //override paint method of panel
            public void paint(final Graphics g) {
                //draw the image
                if (image != null) {
                    g.drawImage(image, 0, 0, this);
                }
            }

        };

        JFrame f = new JFrame();

        f.getContentPane().add(panel);

        int width = image.getWidth(null);
        int height = image.getHeight(null);

        //show frame
        f.setBounds(0, 0, width, height);
        f.setVisible(true);
    }
}
