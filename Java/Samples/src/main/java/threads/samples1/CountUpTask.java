package threads.samples1;

public class CountUpTask implements Runnable {

    private Counter counter;

    public CountUpTask(Counter c) {
        this.counter = c;
    }

    // run メソッドで 100000 回だけカウントアップする
    public void run() {
        for (int i = 0; i < 100000; i++) {
            this.counter.countUp();
        }
    }
}
