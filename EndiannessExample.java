public class EndiannessExample {
    public static void main(String[] args) {
        int originalValue = 0x12345678;  // Beispielwert im Big-Endian-Format

        System.out.println("Originalwert (Big Endian): " + Integer.toHexString(originalValue));

        int littleEndianValue = swapEndianness(originalValue);

        System.out.println("Wert nach Little-Endian-Konvertierung: " + Integer.toHexString(littleEndianValue));
    }

    private static int swapEndianness(int value) {
        return Integer.reverseBytes(value);
    }
}
