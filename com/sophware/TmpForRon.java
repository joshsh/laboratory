
import com.sun.org.apache.xml.internal.security.utils.Base64;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TmpForRon {
    private static final MessageDigest SHA1_DIGEST;

    static {
        try {
            SHA1_DIGEST = MessageDigest.getInstance("SHA1");
        } catch (NoSuchAlgorithmException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static void main(final String[] args) throws Exception {
        String password = "guesteditor";
        System.out.println("password = " + password);
        String passSha1 = sha1SumOf(password);
        System.out.println("sha1 sum of password (base 16): " + passSha1);
        BigInteger passInt = new BigInteger(passSha1, 16);
        System.out.println("sha1 sum of password (base 10): " + passInt);
        System.out.println("sha1 sum of password (base 64): " + Base64.encode(passInt.toByteArray()));

    }

    private static String sha1SumOf(final String key) {
        SHA1_DIGEST.update(key.getBytes());
        String hash = "";
        byte[] digest = SHA1_DIGEST.digest();
        for (byte b : digest) {
            String hex = Integer.toHexString(b);
            if (hex.length() == 1)
                hex = "0" + hex;
            hex = hex.substring(hex.length() - 2);
            hash = hash + hex;
        }
        return hash;
    }
}
