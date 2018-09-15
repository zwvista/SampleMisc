package threads.samples1;

public class CountUpTest2 {

    public static void main(String[] args) throws InterruptedException {
        Counter counter = new Counter();
        CountUpTask task = new CountUpTask(counter);
        Thread thread = new Thread(task);

        // カウントを開始する
        thread.start();

        // カウントアップが終了するまで待機
        thread.join();

        // カウンタの値を表示する
        System.out.println(counter.get());
    }
}
