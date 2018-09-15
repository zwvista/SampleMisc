package threads.samples1;

public class BusyLoop implements Runnable {

    private boolean ok;

    public BusyLoop() {
        super();
        this.ok = false;
    }

    public void run() {
        // 何らかの処理

        // this.ok が false の間はここで止まる
        while (this.ok == false) {
            // 何もしない
            //　Thread.sleep(100);
            try {
                Thread.sleep(1);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // this.ok が true になると処理を行う

        // 何らかの処理
    }

    // this.ok を true にするメソッド
    public void ok() {
        this.ok = true;
    }
}
