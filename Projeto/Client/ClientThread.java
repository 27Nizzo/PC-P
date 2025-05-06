import java.io.*;
import java.net.Socket;
import Client.Data;

public class ClientThread extends Thread {
    private final Socket socket;
    private final Data data;
    private final BufferedReader in;
    private final PrintWriter out;

    public ClientThread(Socket socket, Data data) throws IOException {
        this.socket = socket;
        this.data = data;
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintWriter(socket.getOutputStream(), true);
    }

    @Override
    public void run() {
        while (true) {
            try {
                // Espera que a UI defina uma opção
                while (data.option == null)
                    data.waitPostman.await();

                switch (data.option) {
                    case "LOGIN":
                        handleLogin();
                        break;
                    case "REGISTER":
                        handleRegister();
                        break;
                    case "QUIT":
                        handleQuit();
                    default:
                        data.response = Response.ERROR;
                }

                // Limpa a opção após execução
                data.option = null;

                // Notifica a UI que a resposta está pronta
                data.waitScreen.signal();

            } catch (Exception e) {
                e.printStackTrace();
                data.response = Response.ERROR;
                data.waitScreen.signal(); // Evita bloqueio se der erro
            } finally {
                data.lock.unlock();
            }
        }
    }

    private void handleLogin() throws IOException {
        // Exemplo de comando para o servidor (adapta à tua linguagem de protocolo)
        out.println("LOGIN " + data.username + " " + data.password);
        String response = in.readLine();

        if ("OK".equals(response)) {
            data.response = Response.DONE;
        } else {
            data.response = Response.ERROR;
        }
    }

    private void handleRegister() throws IOException {
       out.println("REGISTER " + data.username + " " + data.password);
    }

    private void handleQuit() throws IOException {
        socket.close();
        return;
    }
}
