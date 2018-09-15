package threads.samples1;

public class ThreadInterrupt {

    public static void main(String[] args) throws InterruptedException {
        LazyTask task = new LazyTask();
        Thread lazyThread = new Thread(task);

        // スレッドを開始する
        lazyThread.start();

        // 5秒間だけ待つ
        Thread.sleep(5000);

        // 待ちきれないので終了
        lazyThread.interrupt();
    }
}