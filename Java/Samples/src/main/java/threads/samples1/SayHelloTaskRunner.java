package threads.samples1;

public class SayHelloTaskRunner {

    public static void main(String[] args) {
        // Runnable を継承したクラスをインスタンス化し
        SayHelloTask task = new SayHelloTask();

        // Thread をインスタンス化する際に引数に渡す
        Thread thread = new Thread(task);

        // Thread の start() メソッドを呼び出すと、task.run() が呼ばれる
        thread.start();
    }
}