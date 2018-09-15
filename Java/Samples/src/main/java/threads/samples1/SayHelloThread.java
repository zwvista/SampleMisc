package threads.samples1;

public class SayHelloThread extends Thread {
    public void run() {
        // "Hello, world!" と 10 回表示する
        for (int i = 1; i <= 10; i++) {
            System.out.println("Hello, world! - " + i);
        }
    }
}
