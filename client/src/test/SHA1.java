package test;

import java.security.MessageDigest;

public class SHA1 {
    public static String hash(String data) throws Exception {
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(data.getBytes());

        byte byteData[] = md.digest();

        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < byteData.length; i++) {
            sb.append(Integer.toString((byteData[i] & 0xff) + 0x100, 16).substring(1));
        }

        return sb.toString();
    }
}
