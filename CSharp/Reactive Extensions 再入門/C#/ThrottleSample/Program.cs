using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;
using System.Reactive.Linq;
using System.Threading;

namespace ThrottleSample
{
    class Program
    {
        static void Main(string[] args)
        {
            ThrottleSample();
        }

        static void ThrottleSample()
        {
            Console.WriteLine("#ThrottleSample");
            {
                var source = new Subject<int>();
                // 500ms値が発行されなかったら最後に発行された値を後続に流す
                source
                    .Throttle(TimeSpan.FromMilliseconds(500))
                    // 渡ってきた値を時間つきで表示
                    .Subscribe(i =>
                        Console.WriteLine("{0:HH:mm:ss.fff} {1}", DateTime.Now, i));

                // 100ms間隔で値を発行
                foreach (var i in Enumerable.Range(1, 10))
                {
                    // 発行した値を出力しておく
                    Console.WriteLine("{0:HH:mm:ss.fff} OnNext({1})",DateTime.Now, i);
                    source.OnNext(i);
                    Thread.Sleep(100);
                }

                // 2000ms sleep
                Console.WriteLine("{0:HH:mm:ss.fff} Sleep(2000)", DateTime.Now);
                Thread.Sleep(2000);

                // 100ms間隔で値を発行
                foreach (var i in Enumerable.Range(1, 5))
                {
                    // 発行した値を出力しておく
                    Console.WriteLine("{0:HH:mm:ss.fff} OnNext({1})", DateTime.Now, i);
                    source.OnNext(i);
                    Thread.Sleep(100);
                }

                // 2000ms sleep
                Console.WriteLine("{0:HH:mm:ss.FFF} Sleep(2000)", DateTime.Now);
                Thread.Sleep(2000);

            }
            Console.WriteLine("---------------------------------");
        }
    }
}
