package timeacc;

public class StopWatch {
    private IAcc _acc;
    private long _start;
    
    public StopWatch(IAcc acc) {
        _acc = acc;
        _start = 0;
    }

    public void start() {
        _start = System.nanoTime();
    }

    public void stop() {
        _acc.accumulateNanoSecondsSince(_start);
    }
}
