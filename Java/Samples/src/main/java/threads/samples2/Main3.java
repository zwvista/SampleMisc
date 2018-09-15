package threads.samples2;

public class Main3 {

    public static void main(String[] args) {
        try {
            for (int i = 0; i < 5; i++) {
                long start = System.currentTimeMillis();
                Thread.sleep(1000);
                long stop = System.currentTimeMillis();
                System.out.println((stop - start) + " [ms]");
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}