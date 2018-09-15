package threads.samples1;

public class IgnoreInterrupted implements Runnable {

    public void run() {
        for (int i = 1; i <= 10; i++) {
            try {
                Thread.sleep(1000);
            }
            catch (InterruptedException e) {
                // 例外を無視
            }
            System.out.println("Hello, world! - " + i);
        }
    }
}