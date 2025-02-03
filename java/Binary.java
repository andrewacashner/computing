package com.andrewcashner.binary;

/** 
 * Utilities for binary arithmetic and conversions
 *
 * @author Andrew Cashner
 * @version 2025/01/23
 */
public class Binary {
    public static int binaryToDecimal(String binary) {
        return Integer.parseInt(binary, 2);
    }

    public static String decimalToBinary(int decimal) {
        return Integer.toBinaryString(decimal);
    }

    public static String binaryAdd(String binN, String binM) {
        return decimalToBinary(
                binaryToDecimal(binN) + 
                binaryToDecimal(binM));
    }

    public static String binaryMultiply(String binN, String binM) {
        return decimalToBinary(
                binaryToDecimal(binN) *
                binaryToDecimal(binM));
    }

    public static String decimalToHex(int decimal) {
        return Integer.toHexString(decimal);
    }
    
    public static String binaryToHex(String binary) {
        return decimalToHex(binaryToDecimal(binary));
    }

    public static int hexToDecimal(String hex) {
        return Integer.parseInt(hex, 16);
    }

    public static String hexToBinary(String hex) {
        return decimalToBinary(hexToDecimal(hex));
    }

    public static String hexAdd(String h1, String h2) {
        return decimalToHex(hexToDecimal(h1) + hexToDecimal(h2));
    }

    public static String binaryAND(String bin1, String bin2) {
        return decimalToBinary(binaryToDecimal(bin1) & 
                binaryToDecimal(bin2));
    }

    public static String binaryOR(String bin1, String bin2) {
        return decimalToBinary(binaryToDecimal(bin1) |
                binaryToDecimal(bin2));
    }

    public static String binaryXOR(String bin1, String bin2) {
        return decimalToBinary(binaryToDecimal(bin1) ^
                binaryToDecimal(bin2));
    }

}
