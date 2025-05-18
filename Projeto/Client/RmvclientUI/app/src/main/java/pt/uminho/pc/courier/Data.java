package pt.uminho.pc.courier;
import java.util.concurrent.locks.*;

public class Data {
    public Lock lock = new ReentrantLock();
    public Condition waitPostman = lock.newCondition();
    public Condition waitScreen = lock.newCondition();
    public String option; 
    public boolean enter_queue;

    public Response response = Response.NOTHING;

    public String username = "";
    public String password = "";
    // ??leaderboard??
}

