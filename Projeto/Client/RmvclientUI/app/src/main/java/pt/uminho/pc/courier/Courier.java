package pt.uminho.pc.courier;

import pt.uminho.pc.components.GameBoard;
import pt.uminho.pc.components.Mouse;
import pt.uminho.pc.courier.Exceptions.*;
import java.io.IOException;

public class Courier implements Runnable {
    private final ClientTCP cTcp;
    private final Mouse mouse;
    private final Data data;
    private final GameBoard board;
    
    public Courier(ClientTCP cTcp, Mouse mouse, GameBoard board, Data data) {
        this.cTcp = cTcp;
        this.mouse = mouse;
        this.data = data;
        this.board = board;
    }

    public void run() {
        while (true) {
            data.lock.lock();
            try {
                data.waitPostman.await();
                handleOption();
            } catch (Exception e) {
                handleError();
            } finally {
                data.waitScreen.signal();
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
    
    private void handleLogin() throws InvalidPassword, InvalidAccount, IOException {
        cTcp.login(data.username, data.password);
        data.response = Response.DONE;
    }

    private void handleCreateAccount() throws UserExists, InvalidPassword, IOException {
        cTcp.create_account(data.username, data.password);
        clearCredentials();
        data.response = Response.DONE;
    }

    private void handleDeleteAccount() throws InvalidPassword, InvalidAccount, IOException {
        //cTcp.remove_account(data.username, data.password);
        clearCredentials();
        data.response = Response.DONE;
    }

    private void handleLogout() throws InvalidPassword, InvalidAccount, IOException {
        //cTcp.logout();
        clearCredentials();
        data.response = Response.DONE;
    }

    private void handleQueues() throws IOException {
        //data.enter_queue = cTcp.enter_queue(); // dar fix aos queues porque ainda está join
        data.response = Response.DONE;
    }

    private void handlePlay() throws IOException, InvalidAccount, FullServer {
        //cTcp.join(data.username, data.password);
        data.response = Response.DONE;
        data.option = "LEADERBOARD";

        new Thread(() -> {
            try {
                String response = cTcp.receive();
                if ("start".equals(response)) {
                    data.option = "GAME";
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }).start();
    }

    private void handleGame() throws IOException {
        String response = cTcp.receive();

        if (response == null || "defeat".equals(response) || "winner".equals(response)) {
            data.option = "LOGGED_IN";
            data.response = Response.SWITCH;
        } else {
            board.update(data.username, response);
            //cTcp.mouse(mouse.toString());
            data.response = Response.DONE;
        }
    }

    private void handleLeaveGame() {
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
