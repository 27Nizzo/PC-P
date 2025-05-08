package Client;

public class Tuple<S1, S2> {
    public S1 first;
    public S2 second;

    public Tuple(S1 fst, S2 snd) {
        this.first = fst;
        this.second = snd;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(first);
        sb.append(" ");
        sb.append(second);
        return sb.toString();
    }

    
}
