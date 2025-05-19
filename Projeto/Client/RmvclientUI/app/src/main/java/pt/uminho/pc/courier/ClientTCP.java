package pt.uminho.pc.courier;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.InputStream;
import java.net.Socket;
import java.util.*;
import java.nio.ByteBuffer;

public class ClientTCP {
    private Socket s; 
    private BufferedReader in;
    private PrintWriter out;
    private OutputStream outStream;
    private InputStream inStream;
    StringBuilder sb = new StringBuilder();

    public ClientTCP(String host, int port) throws IOException {
        this.s = new Socket(host, port);
        this.in = new BufferedReader(new InputStreamReader(s.getInputStream()));
        this.out = new PrintWriter(s.getOutputStream());
        this.outStream = s.getOutputStream();
        this.inStream = s.getInputStream();
    }

    public void send(String message) {
        // Parse the message to create appropriate Erlang terms
        String[] parts = message.split("#");
        if (parts.length >= 2) {
            String command = parts[0];
            String[] args = Arrays.copyOfRange(parts, 1, parts.length);
            
            // Convert space-separated arguments to separate arguments
            List<String> allArgs = new ArrayList<>();
            for (String arg : args) {
                String[] spaceSplit = arg.split(" ");
                for (String s : spaceSplit) {
                    if (!s.trim().isEmpty()) {
                        allArgs.add(s.trim());
                    }
                }
            }
            
            sendErlangTuple(command, allArgs.toArray(new String[0]));
        } else {
            // For simple messages, just send the plain text
            out.println(message);
            out.flush();
        }
    }

    // Sends a tuple {Atom, String, String, ...} in Erlang External Term Format
    private void sendErlangTuple(String atom, String... args) {
        try {
            // Encoding basic Erlang tuple {atom, string, string, ...}
            ByteBuffer buffer = ByteBuffer.allocate(1024);
            buffer.put((byte)131); // ETF version
            buffer.put((byte)104); // Small tuple header
            buffer.put((byte)(args.length + 1));   // Arity (args + atom)
            
            // First element: atom
            buffer.put((byte)100); // atom_ext
            buffer.putShort((short)atom.length());
            buffer.put(atom.getBytes());
            
            // Remaining elements: binary strings
            for (String arg : args) {
                if (arg != null) {
                    buffer.put((byte)107); // string_ext
                    buffer.putShort((short)arg.length());
                    buffer.put(arg.getBytes());
                } else {
                    // Handle null as empty string
                    buffer.put((byte)107); // string_ext
                    buffer.putShort((short)0);
                }
            }
            
            // Write only the filled portion of the buffer
            byte[] data = new byte[buffer.position()];
            buffer.flip();
            buffer.get(data);
            
            outStream.write(data);
            outStream.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String receive() throws IOException {
        // Read ETF format version byte
        int versionByte = inStream.read();
        if (versionByte != 131) {
            // Not ETF format, read as plain text
            return in.readLine();
        }
        
        // Read tag byte to determine the type
        int tag = inStream.read();
        
        // Handle different Erlang term types
        if (tag == 100) { // Atom
            // Read atom length
            int length = (inStream.read() << 8) | inStream.read();
            byte[] bytes = new byte[length];
            readFully(bytes);
            return new String(bytes);
        } else if (tag == 107) { // String
            // Read string length
            int length = (inStream.read() << 8) | inStream.read();
            byte[] bytes = new byte[length];
            readFully(bytes);
            return new String(bytes);
        } else {
            // For other types, just return the tag as a string
            return "Received tag: " + tag;
        }
    }

    /**
     * Reads the exact number of bytes from the input stream into the provided buffer
     */
    private void readFully(byte[] buffer) throws IOException {
        int bytesRead = 0;
        int remaining = buffer.length;
        while (remaining > 0) {
            int count = inStream.read(buffer, bytesRead, remaining);
            if (count == -1) {
                throw new IOException("End of stream reached before reading all data");
            }
            bytesRead += count;
            remaining -= count;
        }
    }

    public void create_account(String username, String password) throws IOException, Exceptions.InvalidPassword, Exceptions.UserExists {
        sendErlangTuple("register", username, password);

        String response = receive();
        System.out.println(response);
        switch (response) {
            case "done": break;
            case "user_exists": throw new Exceptions.UserExists("User already exists.");
            case "invalid_password": throw new Exceptions.InvalidPassword("Invalid password.");
        }
    }

    public void login(String username, String password) throws IOException, Exceptions.InvalidPassword, Exceptions.InvalidAccount {
        sendErlangTuple("login", username, password);
        
        String response = receive();
        switch (response) {
            case "Login#Success": break;
            case "Login#InvalidPassword": throw new Exceptions.InvalidPassword("Invalid password");
            case "Login#InvalidAccount":  throw new Exceptions.InvalidAccount("Invalid account");    
        }
    }

    public void logout(String username, String password) throws IOException {
        sendErlangTuple("logout", username, password);
        
        String response = receive();
        switch (response) {
            case "Login#Success": break; 
        }
    }
}
