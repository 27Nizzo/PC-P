package Client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

public class Client {
    private Socket s; 
    private BufferedReader in;
    private PrintWriter out;
    StringBuilder sb = new StringBuilder();

    public Client(String host, int port) throws IOException {
        this.s = new Socket(host, port);
        this.in = new BufferedReader(new InputStreamReader(s.getInputStream()));
        this.out = new PrintWriter(s.getOutputStream());
    }

    public void send(String message) {
        out.println(message);
        out.flush();
    }

    public String receive() throws IOException {
        return in.readLine();
    }

    public void create_account(String username, String password) throws IOException, Exceptions.InvalidPassword, Exceptions.UserExists {
        sb.append("create_account#").append(username).append(" ").append(password);
        send(sb.toString());
        sb.setLength(0);

        String response = receive();
        switch (response) {
            case "done": break;
            case "user_exists": throw new Exceptions.UserExists("User already exists.");
            case "invalid_password": throw new Exceptions.InvalidPassword("Invalid password.");
        }
    }
    

    public void login(String username, String password) throws IOException, Exceptions.InvalidPassword, Exceptions.InvalidAccount {
        sb.append("login#").append(username).append(" ").append(password);
        send(sb.toString());
        sb.setLength(0);

        String response = receive();
        switch (response) {
            case "Login#Success": break;
            case "Login#InvalidPassword": throw new Exceptions.InvalidPassword("Invalid password");
            case "Login#InvalidAccount":  throw new Exceptions.InvalidAccount("Invalid account");    
        }
    }

    public void logout() throws IOException, Exceptions.InvalidPassword, Exceptions.InvalidAccount {
        sb.append("Logout#");
        send(sb.toString());
        sb.setLength(0);

        String response = receive();
        switch(response) {
            case "Logout#Success": break;
            case "Logout#InvalidPassword": throw new Exceptions.InvalidPassword("Invalid password");
            case "Logout#InvalidAccount":  throw new Exceptions.InvalidAccount("Invalid account");    
        }
    }

    public void join(String username, String password) throws IOException, Exceptions.InvalidAccount, Exceptions.FullServer {
        sb.append("Join#").append(username).append(" ").append(password);
        send(sb.toString());
        sb.setLength(0);

        String response = receive();
        switch (response) {
            case "Join#Success": break;
            case "Join#FullServer": throw new Exceptions.FullServer("Server is full");
            case "Join#Exceptions.InvalidAccount": throw new Exceptions.InvalidAccount("Invalid account");
        }
    }

    public void remove_account(String username, String password) throws IOException, Exceptions.InvalidPassword, Exceptions.InvalidAccount {
        sb.append("remove_account#").append(username).append(" ").append(password);
        send(sb.toString());
        sb.setLength(0);

        String response = receive();
        switch (response) {
            case "done": break;
            case "invalid_account": throw new Exceptions.InvalidAccount("Invalid account.");
            case "invalid_password": throw new Exceptions.InvalidPassword("Invalid password.");
        }
    }

    public Set<String> online() throws IOException {
        sb.append("online#");
        send(sb.toString());
        sb.setLength(0);

        String response = receive();
        String[] playerStrings = response.split(" ");
        Set<String> users = new TreeSet<>();
        for(String user : playerStrings) users.add(user);
        return users;   
    }
}
