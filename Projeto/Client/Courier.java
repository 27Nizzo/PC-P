package Client;
import java.io.IOException;

import Client.Exceptions.FullServer;
import Client.Exceptions.InvalidAccount;
import Client.Exceptions.InvalidPassword;
import Client.Exceptions.UserExists;

public class Courier implements Runnable {
    private final ClientTCP cTcp;
    private final Mouse mouse;
    private final Data data;
    private final Board board;
    
    public Courier(ClientTCP cTcp, Mouse mouse, Board board, Data data) {
        this.cTcp = cTcp;
        this.mouse = mouse;
        this.data = data;
        this.board = board;
    }
    public void run() {
        while(true) {
            data.lock.lock();
            try {
                data.waitPostman.await();
                handleOption();
            } catch (InterruptedException | IOException e) {
                handleError();
            } finally {
                data.lock.unlock();
            }
        }
    }

    public void handleOption() throws IOException, InvalidPassword, InvalidAccount, UserExists, FullServer {
        switch (data.option) {
            case "USERNAME":
            case "PASSWORD":
                handleLogin();
                break;
            case "YOUR_USERNAME":
            case "YOUR_PASSWORD":
                handleCreateAccount();
                break;
            case "DELETE_ACCOUNT":
                handleDeleteAccount();
                break;
            case "LOGOUT":
                handleLogout();
                break;
            case "ENTER_QUEUE":
                handleQueues();
                break;
            case "PLAY":
                handlePlay();
                break;
            case "GAME":
                handleGame();
                break;
            case "LEAVE_GAME":
                handleLeaveGame();
                break;
            case "QUIT_GAME":
                handleQuitGame();
                break;
        }
        data.waitScreen.signal();
    }
private void handleLogin() throws InvalidPassword, InvalidAccount {
        cTcp.login(data.username, data.password);
        data.response = Response.DONE;
    }

    private void handleCreateAccount() throws UserExists {
        cTcp.create_account(data.username, data.password);
        clearCredentials();
        data.response = Response.DONE;
    }

    private void handleDeleteAccount() throws InvalidPassword, InvalidAccount {
        cTcp.remove_account(data.username, data.password);
        clearCredentials();
        data.response = Response.DONE;
    }

    private void handleLogout() throws InvalidPassword, InvalidAccount {
        cTcp.logout();
        clearCredentials();
        data.response = Response.DONE;
    }

    private void handleQueues() throws IOException {
        data.enter_queue = cTcp.enter_queue(); // dar fix aos queues porque ainda está join
        data.response = Response.DONE;
    }

    private void handlePlay() throws IOException, InvalidPassword, InvalidAccount {
        cTcp.join(data.username, data.password);
        data.response = Response.DONE;
        data.option = State.LEADERBOARD;

        new Thread(() -> {
            try {
                String response = cTcp.receive();
                if ("start".equals(response)) {
                    data.option = State.GAME;
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }).start();
    }

    private void handleGame() throws IOException {
        String response = cTcp.receive();

        if (response == null || "defeat".equals(response) || "winner".equals(response)) {
            data.option = State.LOGGED_IN;
            data.response = Response.SWITCH;
        } else {
            board.setBoard(data.username, response);
            cTcp.mouse(mouse.toString());
            data.response = Response.DONE;
        }
    }

    private void handleLeaveGame() throws IOException {
        cTcp.send("leave#");
        data.response = Response.DONE;
    }

    private void handleQuitGame() throws IOException {
        cTcp.send("leave#");
        cTcp.receive();
        data.response = Response.DONE;
    }

    private void handleError() {
        data.response = Response.ERROR;
        clearCredentials();
        data.waitScreen.signal();
    }

    private void clearCredentials() {
        data.username = "";
        data.password = "";
    }

}