package threads.samples1;

public class ThreadSample extends Thread {

    private final String printed;

    public ThreadSample(String printed) {
        this.printed = printed;
    }

    public void run() {
        // コンストラクタに指定された引数を10回表示する
        for (int i = 1; i <= 10; i++) {
            System.out.println(i + ":" + this.printed);
        }
    }

    public static void main(String[] args) {
        ThreadSample thread1 = new ThreadSample("ほげ");
        ThreadSample thread2 = new ThreadSample("ふー");
        thread1.start();
        thread2.start();
    }
}
