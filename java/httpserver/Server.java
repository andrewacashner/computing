package com.andrewcashner.server;

import com.sun.net.httpserver.*;
import java.net.*;
import java.io.*;

/**
 * Simple HTTP server for REST API
 * @author Andrew Cashner
 * @version 2025/01/09
 */
public class Server {
    public static void main(String[] args) {
        try  {
            HttpServer server = HttpServer.create(
                    new InetSocketAddress(8000), 0);
            server.createContext("/evaluate", new MyHandler());
            server.createContext("/", 
            server.setExecutor(null);
            server.start();
        }
        catch (IOException e) {
            System.out.println("IO Exception: " + e.getMessage());
        }
    }
}
