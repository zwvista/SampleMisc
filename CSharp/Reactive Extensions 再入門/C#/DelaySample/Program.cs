using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;
using System.Reactive.Linq;
using System.Threading;

namespace DelaySample
{
    class Program
    {
        static void Main(string[] args)
        {
            DelaySample();
        }

        static void DelaySample()
        {
            Console.WriteLine("#DelaySample");
            {
                var source = new Subject<int>();
                source
                    // 10秒遅延させる
                    .Delay(TimeSpan.FromSeconds(10))
                    // 値を時間つきで表示させる
                    .Subscribe(i =>
                        Console.WriteLine("{0:HH:mm:ss.fff} {1}", DateTime.Now, i));

                // 1秒間隔で1～5の値を発行
                foreach (var i in Enumerable.Range(1, 5))
                {
                    Console.WriteLine("{0:HH:mm:ss.fff} OnNext({1})", DateTime.Now, i);
                    source.OnNext(i);
                    Thread.Sleep(1000);
                }

                // 終了待ち
                Console.WriteLine("Please enter key");
                Console.ReadLine();
            }
            Console.WriteLine("---------------------------------");
        }
    }
}
