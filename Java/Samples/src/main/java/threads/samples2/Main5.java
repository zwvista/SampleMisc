package threads.samples2;

public class Main5 {

    public static void main(String[] args) {
        MultiThread5 mt = new MultiThread5();
        mt.start();

        try {
            for (int i = 0; i < 3; i++) {
                long start = System.currentTimeMillis();
                Thread.sleep(1000);
                mt.interrupt();
                long stop = System.currentTimeMillis();
                System.out.println((stop - start) + " [ms]");
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}

class MultiThread5 extends Thread {
    public void run() {
        for (int i = 0; i < 3; i++) {
            try {
                Thread.sleep(1000);
                System.out.println("スレッド2の" + (i + 1) + "度目の処理");
            } catch (InterruptedException e) {
                System.out.println("割り込まれました");
            }
        }
    }
}