package threads.samples1;

public class ThreadSleep {

    public static void main(String[] args) throws InterruptedException {
        System.out.println("5000ミリ秒だけ休止");
        Thread.sleep(5000);
        System.out.println("休止終了");
    }
}