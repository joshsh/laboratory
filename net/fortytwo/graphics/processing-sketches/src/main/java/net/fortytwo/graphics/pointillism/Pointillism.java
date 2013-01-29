package net.fortytwo.graphics.pointillism;

/**
 This program explores the perception of a "pointillist" field of dots in two
 different styles:
 1) messy -- no collision detection. Dots may overlap
 2) neat -- a minimum distance between dots is enforced
 The result appears to be that the two styles are indeed perceived differently;
 with the messy style, the eye seems to linger on individual clumps of dots,
 whereas with the neat style, the eye keeps moving.  The neat style certainly
 results in a more homogeneous appearance.
 Note: Xixi claims she prefers the messy style.
 */

import processing.core.PApplet;
import processing.core.PImage;

import java.util.Random;

/**
 * User: josh
 * Date: Dec 6, 2010
 * Time: 6:56:38 PM
 */
public class Pointillism extends PApplet {
    private enum Style { NEAT, MESSY }

    private static Random RANDOM = new Random();

    public void setup() {
        // Use anti-aliasing
        //size(1000, 500, OPENGL);
        //hint(ENABLE_OPENGL_4X_SMOOTH);

        try {
            compareStyles();
            //tryImage();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }

    private void compareStyles() throws Exception {
        background(0);
        size(1000, 500);

        /*
        DotField f1 = new DotField(0.02, 500, false);
        f1.draw(0, 0, 500, 500);
        DotField f2 = new DotField(0.02, 500, true);
        f2.draw(500, 0, 500, 500);
        */
        DotField f1 = new DotField(0.015, 800, Style.MESSY);
        f1.draw(0, 0, 500, 500);
        DotField f2 = new DotField(0.015, 800, Style.NEAT);
        f2.draw(500, 0, 500, 500);
    }

    private void tryImage() throws Exception {
        PImage b;
        //b = loadImage("/Users/josh/tmp/pointillist/1434163590_d1bef5406f_b.jpg");
//        b = loadImage("/Users/josh/tmp/pointillist/400_F_5850901_ByK2aY4YThJolBNCwK0eq1zVhDCpzMf0.jpg");
        b = loadImage("/Users/josh/tmp/pointillist/egg-on-the-white-background.jpg");

        colorMode(HSB);
        //colorMode(RGB, 1.0f);

        background(0);
//        size(b.width, b.height);
        size(2 * b.width, 500);

        DotField f1 = new DotField(0.010, 1000, Style.MESSY);
        f1.drawOnImage(b, 0, 0, 500, 500);

        DotField f2 = new DotField(0.010, 1000, Style.NEAT);
        f2.drawOnImage(b, b.width, 0, 500, 500);
    }

    //public static void main(final String[] args) {
    //    new Pointillism().setup();
    //}

    private class DotField {
        private static final int DOT_COLOR = 0;//100;

        private final double dotRadius;
        private final int numberOfDots;
        private final Style style;

        public DotField(final double dotRadius,
                        final int numberOfDots,
                        final Style style) {
            this.dotRadius = dotRadius;
            this.numberOfDots = numberOfDots;
            this.style = style;
        }

        public void drawOnImage(final PImage img,
                                final int cornerx,
                                final int cornery,
                                int width,
                                int height) throws Exception {
            //image(b, 0, 0);
            fill(255);
            rect(cornerx, cornery, width, height);

            double r = dotRadius * sqrt(width * width + height * height) / sqrt(2);
            double rr = 1.33 * r;

            Point[] dots = new Point[numberOfDots];

            //int min = 1000;
            //int max = 0;

            for (int i = 0; i < numberOfDots; i++) {
                boolean ok = false;
                Point p = null;

                int tries = 0;
                while (!ok) {
                    p = new Point(random(width), random(height));
                    ok = true;

                    int c = img.get((int) p.getX(), (int) p.getY());
                    int b = rgbColorToBrightness(c);
                    if (RANDOM.nextInt(255) < b) {
                        ok = false;
                    } else if (Style.NEAT == style) {
                        for (int j = 0; j < i; j++) {
                            if (p.distance(dots[j]) < rr) {
                                ok = false;
                                tries++;
                                if (tries > 100) {
                                    throw new Exception("too many tries");
                                }
                                break;
                            }
                        }
                    }
                }

                dots[i] = p;

                fill(DOT_COLOR);
                noStroke();
                ellipse(cornerx + (int) p.getX(), cornery + (int) p.getY(), (int) r, (int) r);
            }
            //System.out.println("min: " + min + ", max: " + max);
        }

        private int rgbColorToBrightness(final int c) {
            return ((c & 255) + ((c >> 8) & 255) + ((c >> 16) & 255)) / 3;
        }

        private void showColor(int c) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 4; i++) {
                sb.append((c >> (8 * i)) & 255).append(" ");
            }
            //for (int i = 0; i < 32; i++) {
            //    sb.append(1 == ((c >> i) & 1) ? '1' : '0');
            //}
            System.out.println(sb);
        }

        public void draw(final int cornerx,
                         final int cornery,
                         int width,
                         int height) throws Exception {
            int c = 255;
            fill(c);
            rect(cornerx, cornery, width, height);

            double r = dotRadius * sqrt(width * width + height * height) / sqrt(2);
            double rr = 1.33 * r;

            Point[] dots = new Point[numberOfDots];

            for (int i = 0; i < numberOfDots; i++) {
                boolean ok = false;
                Point p = null;

                int tries = 0;
                while (!ok) {
                    p = new Point(random(width), random(height));
                    ok = true;

                    if (Style.NEAT == style) {
                        for (int j = 0; j < i; j++) {
                            if (p.distance(dots[j]) < rr) {
                                ok = false;
                                tries++;
                                if (tries > 100) {
                                    throw new Exception("too many tries");
                                }
                                break;
                            }
                        }
                    }
                }

                dots[i] = p;

                //System.out.println("x = " + x);
                if (ok) {
                    fill(DOT_COLOR);
                    noStroke();
                    ellipse(cornerx + (int) p.getX(), cornery + (int) p.getY(), (int) r, (int) r);
                }
            }
        }
    }

    private class Point {
        private final float x;
        private final float y;

        public Point(final float x,
                     final float y) {
            this.x = x;
            this.y = y;
        }

        public float getX() {
            return x;
        }

        public float getY() {
            return y;
        }

        public float distance(final Point other) {
            float xdiff = other.x - x;
            float ydiff = other.y - y;
            return sqrt(xdiff * xdiff + ydiff * ydiff);
        }
    }
}
