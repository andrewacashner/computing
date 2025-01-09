package com.andrewcashner.server;

import com.sun.net.httpserver.*;
import java.io.*;
import com.andrewcashner.musarithmetic.*;

import java.util.function.*;

/** 
 * Adapted from
 * <a href="https://docs.oracle.com/en/java/javase/21/docs/api/jdk.httpserver/com/sun/net/httpserver/package-summary.html">Java Docs</a>
 */
public class MyHandler implements HttpHandler {
    public void handle(HttpExchange connection) throws IOException {
        try (InputStream inStream = connection.getRequestBody();
             OutputStream outStream = connection.getResponseBody();) {

            byte[] inBytes = inStream.readAllBytes();
            String message = new String(inBytes);
            System.out.format("GET:\n\'%s\'\n", message);

            try {
                String response = IntervalCalculator.evaluate(message);
                connection.sendResponseHeaders(200, response.length());
                outStream.write(response.getBytes());
                System.out.format("RESPONSE:\'%s\'\n", response);
            }
            catch (IllegalArgumentException e) {
                String response = "Could not evaluate input";
                connection.sendResponseHeaders(400, response.length());
                outStream.write(response.getBytes());
                System.out.format("RESPONSE:\'%s\'\n", response);
            }
        }
        catch (IOException e) {
            throw new IOException("Problem handling HTTP connection", e);
        }
    }
}
