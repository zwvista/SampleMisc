package threads.samples1;

public class CountUpTest1 {

    public static void main(String[] args) {
        // カウンタを作成する
        Counter counter = new Counter();

        // 100000 カウントするためのインスタンスを作成する
        CountUpTask task = new CountUpTask(counter);

        // 100000 カウントするスレッドを作成する
        Thread thread = new Thread(task);

        // カウントを開始する
        thread.start();

        // カウンタの値を表示する
        System.out.println(counter.get());
    }
}