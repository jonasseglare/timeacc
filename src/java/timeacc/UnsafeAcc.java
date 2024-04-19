package timeacc;

public class UnsafeAcc implements IAcc {
    private long _counter = 0;
    private long _totalTimeNs = 0;

    public void reset() {
        _counter = 0;
        _totalTimeNs = 0;
    }

    public void accumulateNanoSeconds(long duration) {
        _counter++;
        _totalTimeNs += duration;
    }
    
    public void accumulateNanoSecondsSince(long start) {
        long end = System.nanoTime();
        accumulateNanoSeconds(end - start);
    }

    public long getCounter() {
        return _counter;
    }

    public long getTotalTimeNs() {
        return _totalTimeNs;
    }

    public String toString() {
        return "UnsafeAcc(avg=" + (1.0e-9*_totalTimeNs)/_counter + " s, n=" + _counter + ", tot=" + 1.0e-9*_totalTimeNs + "s)";
    }
}
