package threads.samples2;

public class Main2 {

    public static void main(String[] args) {
        MultiThread2 mt = new MultiThread2();
        Thread thread = new Thread(mt);
        thread.start();

        for (int i = 0; i < 3; i++) {
            try {
                Thread.sleep(1000);
                System.out.println("スレッド1の" + (i + 1) + "度目の処理");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

}

class MultiThread2 implements Runnable {
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