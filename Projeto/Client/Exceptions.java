package Client;

public class Exceptions {
    public static class InvalidPassword extends Exception {
        public InvalidPassword(String message) {
            super(message);
        }
    }

    public static class InvalidAccount extends Exception {
        public InvalidAccount(String message) {
            super(message);
        }
    }

    public static class FullServer extends Exception {
        public FullServer(String message) {
            super(message);
        }
    }

    public static class InvalidUsername extends Exception {
        public InvalidUsername(String message) {
            super(message);
        }
    }

    public static class InvalidMessage extends Exception {
        public InvalidMessage(String message) {
            super(message);
        }
    }

    public static class InvalidCommand extends Exception {
        public InvalidCommand(String message) {
            super(message);
        }
    }

    public static class UserExists extends Exception {
        public UserExists(String message) {
            super(message);
        }
    }
}
