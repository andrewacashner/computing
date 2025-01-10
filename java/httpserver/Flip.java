package com.andrewcashner.server;

import java.util.stream.*;

public class Flip {
    public static void main(String[] args) {
        String response = "";
        if (args.length > 0) {
            response = Flip.reverse(args[0]);
        }
        System.out.println(response);
    }

    public static String reverse(String input) 
            throws IllegalArgumentException {
        if (input.startsWith("C")) {
            throw new IllegalArgumentException("Thou must not start with C");
        }
        return input.chars()
                .mapToObj(Character::toString)
                .collect(Collectors.toList())
                .reversed()
                .stream().collect(Collectors.joining());
    }
}
