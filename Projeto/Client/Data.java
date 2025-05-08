package Client;
import java.util.Set;
import java.util.concurrent.locks.*;

enum Response {
    NOTHING,
    ERROR,
    SWITCH,
    DONE,
}

public class Data {
    public Lock lock = new ReentrantLock();
    public Condition waitPostman = lock.newCondition();
    public Condition waitScreen = lock.newCondition();
    public State option; //! states para as opções fazer dps RICARDO

    public  Response response = Response.NOTHING;

    public String username = "";
    public String password = "";
    // ??leaderboard??
}
