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

}
