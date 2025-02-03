package com.andrewcashner.vector;

import java.util.Arrays;
import java.util.stream.*;

public class Vector {
    private double[] vector;

    public Vector() {
    }

    public Vector(double... components) {
        this.vector = components;
    }

    public Vector(Vector v) {
        this.vector = Arrays.copyOf(v.vector, v.vector.length);
    }

    public static Vector sum(Vector v, Vector u)
            throws IllegalArgumentException {

        if (v.vector.length != u.vector.length) {
            throw new IllegalArgumentException("Vectors must be equal length");
        }

        double[] sum = new double[v.vector.length];
        for (int i = 0; i < v.vector.length; ++i) {
            sum[i] = v.vector[i] + u.vector[i];
        }
        
        return new Vector(sum);
    }

    public static Vector scale(Vector v, double c)
            throws IllegalArgumentException {

        double[] scaled = new double[v.vector.length];
        for (int i = 0; i < scaled.length; ++i) {
            scaled[i] = v.vector[i] * c;
        }
        return new Vector(scaled);
    }

    public double magnitude() {
        return Math.sqrt(
                 Arrays.stream(this.vector)
                    .map(el -> Math.pow(el, 2))
                    .sum());
    }

    public static Vector normalize(Vector v) {
        return Vector.scale(v, 1 / v.magnitude());
    }

    public static double dotProduct(Vector v, Vector u) 
            throws IllegalArgumentException {

        if (v.vector.length != u.vector.length) {
            throw new IllegalArgumentException("Vectors must be equal length");
        }

        double sum = 0;
        for (int i = 0; i < v.vector.length; ++i) {
            sum += v.vector[i] * u.vector[i];
        }
        return sum;
    }

    public static Vector projection(Vector v, Vector u) {
        return Vector.scale(v, 
                Vector.dotProduct(v, u) / Vector.dotProduct(v, v));
    }

    // crossProduct

    @Override
    public String toString() {
        return "<" + Arrays.stream(this.vector)
                        .mapToObj(Double::toString)
                        .collect(Collectors.joining(", "))
                + ">";
    }
}
