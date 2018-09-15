package threads.samples1;

public class WaitObject {

    // このオブジェクトを使って待機状態に入る
    public synchronized void suspend() throws InterruptedException {
        this.wait();
    }

    // このオブジェクトを使って待機しているスレッドを全て起こす
    public synchronized void resume() {
        this.notifyAll();
    }
}