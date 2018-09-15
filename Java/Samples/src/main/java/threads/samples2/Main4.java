package threads.samples2;

public class Main4 {

    public static void main(String[] args) {
        MultiThread4 mt = new MultiThread4();
        mt.start();

        try {
            System.out.println("別スレッドの処理を待機します。");
            mt.join();
            System.out.println("別スレッドの処理が終わりました。");
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}

class MultiThread4 extends Thread {
    public void run() {
        for (int i = 0; i < 3; i++) {
            try {
                Thread.sleep(1000);
                System.out.println("スレッド2の" + (i + 1) + "度目の処理");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}