package threads.samples1;

public class SayHelloTask implements Runnable {

    public void run() {
        for (int i = 1; i <= 10; i++) {
            System.out.println("Hello, world! - " + i);
        }
    }
}
