package net.fortytwo.graphics;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Point2D;
import java.awt.image.ImageObserver;
import java.awt.image.MemoryImageSource;
import java.awt.image.PixelGrabber;

/**
 * User: josh
 * Date: Sep 22, 2010
 * Time: 11:33:16 PM
 */
public class HyperbolicPlaneEffect {
    private static final Color BACKGROUND_COLOR = Color.WHITE;

    private final Point2D sourceCenter;
    private final double sourceRadius;

    private final int targetRadius;

    private final Image sourceImage;
    private final int[] sourcePixels;
    private final int sourceHeight, sourceWidth;
    private final Image targetImage;


    public HyperbolicPlaneEffect(final Image sourceImage,
                                 final Point2D sourceCenter,
                                 final double sourceRadius,
                                 final int targetRadius) throws Exception {
        this.sourceImage = sourceImage;
        this.sourceCenter = sourceCenter;
        this.sourceRadius = sourceRadius;
        this.targetRadius = targetRadius;

        sourcePixels = imageToPixelArray(sourceImage);
        sourceWidth = sourceImage.getWidth(null);
        sourceHeight = sourceImage.getHeight(null);

        int height = 2 * targetRadius;
        int width = 2 * targetRadius;

        final Color[][] matrix = new Color[height][width];

        double increment = sourceRadius / targetRadius;

        for (int i = 0; i < height; i++) {
            double y = sourceCenter.getY() - sourceRadius + (i * increment);

            for (int j = 0; j < width; j++) {
                Color color;

                if (vectorLength(targetRadius - i, targetRadius - j) < targetRadius) {
                    double x = sourceCenter.getX() - sourceRadius + (j * increment);
                    Point2D source = new Point2D.Double(x, y);
                    //Point2D source = new Point2D.Double(j, i);
                    Point2D target = mapHyperbolic(source);

                    color = getColorAtSource(target);

                    //color = Color.RED;
                } else {
                    color = BACKGROUND_COLOR;
                }

                matrix[i][j] = color;
            }
        }

        targetImage = colorArrayToImage(matrix, height, width);
    }

    public Image getResult() {
        return targetImage;
    }

    private Color getColorAtSource(final Point2D source) {
        int x = ((int) source.getX()) % sourceWidth;
        int y = ((int) source.getY()) % sourceHeight;

        if (x < 0) {
            x += sourceWidth;
        }

        if (y < 0) {
            y += sourceHeight;
        }
        /*
        int x = (int) source.getX();
        if (x < 0 || x >= sourceWidth) {
            System.out.println("out of range!");
            x = 0;
        }

        int y = (int) source.getY();
        if (y < 0 || y >= sourceHeight) {
            System.out.println("out of range!");
            y = 0;
        }
        */

        //System.out.println("(" + x + ", " + y + ")");
        int c = sourcePixels[(y * sourceWidth) + x];
        return new Color(c);
    }

    private Point2D add(Point2D a, Point2D b) {
        return new Point2D.Double(a.getX() + b.getX(), a.getY() + b.getY());
    }

    private Point2D subtract(Point2D a, Point2D b) {
        return new Point2D.Double(a.getX() - b.getX(), a.getY() - b.getY());
    }

    private double norm(final Point2D p) {
        return Math.sqrt((p.getX() * p.getX() + p.getY() * p.getY()));
    }

    private Point2D multiplyBy(final Point2D p,
                               final double factor) {
        return new Point2D.Double(p.getX() * factor, p.getY() * factor);
    }

    private static final double SCALE_FACTOR = 50;

    private Point2D mapHyperbolic(final Point2D source) {
        Point2D diff = subtract(source, sourceCenter);
        double distance = norm(diff);
        double stretchFactor = //distance +
                SCALE_FACTOR * ((1 / sourceRadius) + (1 / (distance - sourceRadius)));

        Point2D offset = multiplyBy(diff, stretchFactor);
        return add(sourceCenter, offset);
    }

    private double vectorLength(final int x,
                                final int y) {
        return Math.sqrt((double) (x * x + y * y));
    }

    public void show() {
        showImage(targetImage);
    }

    private static void showImage(final Image image) {
        ImagePanel panel = new ImagePanel(image);
        JFrame f = new JFrame();
        f.setBackground(BACKGROUND_COLOR);
        f.getContentPane().add(panel);

        int width = image.getWidth(null);
        int height = image.getHeight(null);

        //show frame
        f.setBounds(0, 0, width, height);
        f.setVisible(true);
    }

    private static Image loadImageFromFile(final String filePath) {
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        return toolkit.getImage(filePath);
    }

    private static Image colorArrayToImage(final Color[][] matrix,
                                           final int height,
                                           final int width) {
        int[] bytes = new int[width * height];
        int k = 0;
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                Color c = matrix[i][j];
                bytes[k++] = (c.getAlpha() << 24)
                        | (c.getRed() << 16)
                        | (c.getGreen() << 8)
                        | c.getBlue();
            }
        }

        int off = 0;
        int scan = width;

        return Toolkit.getDefaultToolkit().createImage(
                new MemoryImageSource(width, height, bytes, off, scan));
    }

    private int[] imageToPixelArray(final Image image) throws Exception {
        int x = 0;
        int y = 0;

        int w = image.getWidth(null);
        int h = image.getHeight(null);

        int[] pixels = new int[w * h];

        PixelGrabber pg = new PixelGrabber(image, x, y, w, h, pixels, 0, w);
        pg.grabPixels();

        if ((pg.getStatus() & ImageObserver.ABORT) != 0) {
            throw new Exception("image fetch aborted or errored");
        }

        return pixels;
    }

    ////////////////////////////////////////////////////////////////////////////

    private static void printUsage() {
        System.out.println("Usage:  HyperbolicPlaneEffect [image file] [x] [y] [in radius] [out radius]");
        System.exit(1);
    }

    public static void main_real(final String[] args) {
        try {
            if (5 != args.length) {
                printUsage();
            } else {
                String file = args[0];
                double x = Double.valueOf(args[1]);
                double y = Double.valueOf(args[2]);
                double inRadius = Double.valueOf(args[3]);
                int outRadius = Integer.valueOf(args[4]);

                Image image = loadImageFromFile(file);
                showImage(image);
                Point2D center = new Point2D.Double(x, y);
                HyperbolicPlaneEffect effect = new HyperbolicPlaneEffect(image, center, inRadius, outRadius);
                showImage(effect.getResult());
            }
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        //System.out.println(": " + -113 % 10);

        args = new String[]{"/tmp/test.jpg", "450", "200", "30", "300"};
        main_real(args);
    }
}
