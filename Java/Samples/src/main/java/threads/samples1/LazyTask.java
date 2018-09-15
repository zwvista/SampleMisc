package threads.samples1;

public class LazyTask implements Runnable {

    public void run() {
        try {
            for (int i = 1; i <= 10; i++) {
                // 毎回 1 秒ずつ休止する
                Thread.sleep(1000);

                System.out.println("Hello, world! - " + i);
            }
        }
        catch (InterruptedException e) {
            System.out.println("interrupted");
        }
    }
}