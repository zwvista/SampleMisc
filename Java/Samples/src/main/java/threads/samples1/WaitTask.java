package threads.samples1;

public class WaitTask implements Runnable {

    private WaitObject waitObject;

    public WaitTask(WaitObject wait) {
        this.waitObject = wait;
    }

    public void run() {
        System.out.println("wait 開始");
        try {
            // 待機状態へ
            this.waitObject.suspend();
            System.out.println("wait 終了");
        }
        catch (InterruptedException e) {
            System.out.println("interrupted");
        }
    }
    public static void main(String[] args) throws InterruptedException {
        // 待機用のオブジェクト
        WaitObject wait = new WaitObject();
        WaitTask task = new WaitTask(wait);
        Thread thread = new Thread(task);

        thread.start();

        // 一秒待ってから
        Thread.sleep(1000);

        // 待機状態を解除
        wait.resume();
    }
}