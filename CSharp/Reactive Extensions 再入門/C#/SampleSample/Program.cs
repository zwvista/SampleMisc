using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;

namespace SampleSample
{
    class Program
    {
        static void Main(string[] args)
        {
            SampleSample();
        }

        static void SampleSample()
        {
            Console.WriteLine("#SampleSample");
            {
                var r = new Random();
                var subscriber = Observable
                    // 100msの間隔で
                    .Interval(TimeSpan.FromMilliseconds(100))
                    // 0～99の乱数を発生させる
                    .Select(_ => r.Next(100))
                    // 値が発行されたことを確認するためのダンプ
                    .Do(i => Console.WriteLine("{0:HH:mm:ss} Dump {1}", DateTime.Now, i))
                    // 1秒間隔で最後に発行された値を後続に流す
                    .Sample(TimeSpan.FromMilliseconds(1000))
                    // 購読
                    .Subscribe(
                        // 値を表示
                        i => Console.WriteLine("{0:HH:mm:ss} OnNext {1}", DateTime.Now, i));
                Console.WriteLine("Please enter key");
                Console.ReadLine();
                // 購読終了
                subscriber.Dispose();
            }
            Console.WriteLine("-------------------------------");
            {
                var r = new Random();
                var subscriber = Observable
                    // 10秒間隔で
                    .Interval(TimeSpan.FromSeconds(10))
                    // 乱数を発生させる
                    .Select(_ => r.Next(100))
                    // 発生した乱数を表示
                    .Do(i => Console.WriteLine("{0:HH:mm:ss} Dump {1}", DateTime.Now, i))
                    // 8秒間隔でフィルタリングする
                    .Sample(TimeSpan.FromSeconds(8))
                    .Subscribe(
                        // 渡ってきた値を表示する
                        i => Console.WriteLine("{0:HH:mm:ss} OnNext {1}", DateTime.Now, i));

                Console.WriteLine("Please enter key");
                Console.ReadLine();
                // 購読解除
                subscriber.Dispose();
            }
            Console.WriteLine("-------------------------------");
        }
    }
}
