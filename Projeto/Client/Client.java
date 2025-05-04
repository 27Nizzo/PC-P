package Client;

import java.io.*;
import java.net.*;

public class Client {
    private Socket socket;
    private DataOutputStream out;
    private DataInputStream in;

    public void connect(String host, int port) throws IOException {
        socket = new Socket(host, port);
        out = new DataOutputStream(socket.getOutputStream());
        in = new DataInputStream(socket.getInputStream());
        System.out.println("Ligado ao servidor " + host + ":" + port);
    }

    public void send(String msg) throws IOException {
        out.write(msg.getBytes());
        out.flush();
    }

    public String receive() throws IOException {
        byte[] buffer = new byte[1024]; // Ajusta conforme necessário
        int bytesRead = in.read(buffer);
        if (bytesRead == -1) {
            return null;
        }
        return new String(buffer, 0, bytesRead);
    }

    public void close() throws IOException {
        in.close();
        out.close();
        socket.close();
    }

    public static void main(String[] args) {
        Client client = new Client();
        try {
            client.connect("localhost", 1234);

            // Exemplo: enviar comando de login (como string crua)
            client.send("{login, \"Afonso\", \"1234\"}.\n");

            String response = client.receive();
            System.out.println("Resposta do servidor: " + response);

            client.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
