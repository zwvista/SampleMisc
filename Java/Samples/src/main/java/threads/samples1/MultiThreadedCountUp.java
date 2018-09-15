package threads.samples1;

public class MultiThreadedCountUp {

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