package threads.samples3;

class Counter {

    private int count;

    Counter() {
        this.count = 0;
    }

    public synchronized void countUp() {
        this.count += 1;
    }

    public int get() {
        return this.count;
    }
}

class CountUpTask implements Runnable {

    private Counter counter;

    CountUpTask(Counter c) {
        this.counter = c;
    }

    // run メソッドで 100000 回だけカウントアップする
    public void run() {
        for (int i = 0; i < 100000; i++) {
            this.counter.countUp();
        }
    }
}

public class ThreadTest2 {
    public static void main(String[] args) throws InterruptedException {
        Counter counter = new Counter();

        // カウントアップするスレッドを2つ作る
        CountUpTask countUp = new CountUpTask(counter);
        Thread t1 = new Thread(countUp);
        Thread t2 = new Thread(countUp);

        // 合計200000回カウントアップする
        t1.start();
        t2.start();

        // カウントアップが終わるまで待つ
        t1.join();
        t2.join();

        // 結果を表示
        System.out.println(counter.get());
    }
}
