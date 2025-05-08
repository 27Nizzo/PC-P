package Client;

public class Client {
    public static void main(String[] args) {
        new Client().start(args);
    }

    private void start(String[] args) {
        if (args.length < 2) {
            printUsageAndExit();
        }

        String host = args[0];
        int port = parsePort(args[1]);

        try {
            runClient(host, port);
        } catch (Exception e) {
            System.err.println("Erro ao iniciar cliente: " + e.getMessage());
        }
    }

    private void runClient(String host, int port) throws Exception {
        ClientTCP clientTCP = new ClientTCP(host, port);
        Mouse mouse = new Mouse();
        Board board = new Board(); //TODO: Por fazer RICARDO
        Data data = new Data();

        Thread screenThread = new Thread(new Screen(mouse, board, data)); //TODO: Por fazer RICARDO
        Thread courierThread = new Thread(new Courier(clientTCP, mouse, board, data));

        screenThread.start();
        courierThread.start();
    }

    private int parsePort(String portStr) {
        try {
            return Integer.parseInt(portStr);
        } catch (NumberFormatException e) {
            System.err.println("Porto inválido: " + portStr);
            System.exit(1);
            return -1; // Nunca chega aqui
        }
    }

    private void printUsageAndExit() {
        System.out.println("Uso incorreto:");
        System.out.println("Uso: Client [host] [port]");
        System.exit(1);
    }
}
