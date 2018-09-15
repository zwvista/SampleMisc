package threads.samples3;

public class ThreadTest {
    public static void main(String[] args) {
        // 创建 Thread 子类的实例
        Thread1 t1 = new Thread1();
        // 创建实现了 Runnable 接口的类的实例，将其作为参数传给 Thread 类的实例
        Thread t2 = new Thread(new Runnable1());
        // 通过调用 start 方法启动线程
        t1.start();
        t2.start();
    }
}

class Thread1 extends Thread {
    @Override
    public void run() {
        for(int i = 0; i < 3; i++) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            System.out.println(i + " in Thread1");
        }
    }
}

class Runnable1 implements Runnable {
    @Override
    public void run() {
        for(int i = 0; i < 3; i++) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            System.out.println(i + " in Runnable1");
        }
    }
}

/*
0 in Thread1
0 in Runnable1
1 in Runnable1
1 in Thread1
2 in Runnable1
2 in Thread1
*/
