package net.fortytwo.homework.operatingsystems.memory;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Oct 17, 2008
 * Time: 9:42:56 PM
 * To change this template use File | Settings | File Templates.
 */
public class Assignment2 {
    public static void main(final String[] args) throws Exception {

    }

    /*
    0204 (000400505040070060200)n 000204 [000204(00070401611617151161211)n-i 001412116151151211]n 00
     */
    private static int[] createPageReferenceString(final int n) {
        StringBuilder sb = new StringBuilder();
        sb.append("0204");
        for (int i = 0; i < n; i++) {
            sb.append("000400505040070060200");
        }
        sb.append("000204");
        for (int i = 0; i < n; i++) {
            sb.append("000204");
            for (int j = i; j < n; j++) {
                sb.append("00070401611617151161211");
            }

            sb.append("001412116151151211");
        }
        sb.append("00");

        int[] result = new int[sb.length()];
        int i = 0;
        for (char c : sb.toString().toCharArray()) {
            result[i++] = new Integer(c).intValue();
        }

        return result;
    }
}
