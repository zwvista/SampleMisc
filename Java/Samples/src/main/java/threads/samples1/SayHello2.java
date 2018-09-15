package threads.samples1;

public class SayHello2 {

    public static void main(String[] args) {
        SayHelloThread thread = new SayHelloThread();

        // 新しいスレッドで SayHelloThread.run() を実行する
        thread.start();

        // "こんにちは、世界！" と 10 回表示する
        for (int i = 1; i <= 10; i++) {
            System.out.println("こんにちは、世界！ - " + i);
        }
    }
}