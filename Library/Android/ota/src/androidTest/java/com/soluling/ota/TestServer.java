package com.soluling.ota;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

// A stand-in for Soluling's CDN and API: serves the responses set with put, and counts the requests
final class TestServer
{
  static final class Response
  {
    final int status;
    final String body;

    Response(int status, String body)
    {
      this.status = status;
      this.body = body;
    }
  }

  private final ServerSocket socket;
  private final Map<String, Response> responses = new ConcurrentHashMap<>();
  private final Map<String, AtomicInteger> counts = new ConcurrentHashMap<>();
  private volatile boolean failAll;

  TestServer() throws IOException
  {
    socket = new ServerSocket(0, 50, InetAddress.getByName("127.0.0.1"));

    Thread thread = new Thread(this::serve, "TestServer");
    thread.setDaemon(true);
    thread.start();
  }

  String url()
  {
    return "http://127.0.0.1:" + socket.getLocalPort();
  }

  void put(String path, int status, String body)
  {
    responses.put(path, new Response(status, body));
  }

  void remove(String path)
  {
    responses.remove(path);
  }

  // Every request gets 500
  void setFailAll(boolean value)
  {
    failAll = value;
  }

  int count(String path)
  {
    AtomicInteger count = counts.get(path);
    return (count != null) ? count.get() : 0;
  }

  void close() throws IOException
  {
    socket.close();
  }

  private void serve()
  {
    while (!socket.isClosed())
    {
      try (Socket client = socket.accept())
      {
        BufferedReader reader = new BufferedReader(new InputStreamReader(client.getInputStream(), StandardCharsets.UTF_8));
        String line = reader.readLine();

        if (line == null)
          continue;

        String path = line.split(" ")[1];

        // Skip the headers
        while ((line = reader.readLine()) != null && !line.isEmpty())
        {
        }

        counts.computeIfAbsent(path, key -> new AtomicInteger()).incrementAndGet();

        Response response = failAll ? new Response(500, "Error") : responses.get(path);

        if (response == null)
          response = new Response(404, "Not found");

        byte[] body = response.body.getBytes(StandardCharsets.UTF_8);
        String header = "HTTP/1.1 " + response.status + " X\r\nContent-Length: " + body.length + "\r\nConnection: close\r\n\r\n";
        OutputStream output = client.getOutputStream();

        output.write(header.getBytes(StandardCharsets.UTF_8));
        output.write(body);
        output.flush();
      }
      catch (IOException e)
      {
        // Closed, or a broken request
      }
    }
  }
}
