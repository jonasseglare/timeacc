package timeacc;

public interface IAcc {
    void reset();
    void accumulateNanoSeconds(long duration);
    void accumulateNanoSecondsSince(long start);
    long getCounter();
    long getTotalTimeNs();
}
